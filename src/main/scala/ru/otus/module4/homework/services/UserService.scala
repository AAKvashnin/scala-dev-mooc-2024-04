package ru.otus.module4.homework.services

import io.circe._
import io.circe.generic.semiauto._
import ru.otus.module4.homework.dao.entity.{Role, RoleCode, User}
import ru.otus.module4.homework.dao.repository.UserRepository
import ru.otus.module4.phoneBook.db
import zio.Has
import zio.RIO
import zio.ZLayer
import zio.macros.accessible

@accessible
object UserService{
    type UserService = Has[Service]

    trait Service{
        def listUsers(): RIO[db.DataSource, List[User]]
        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
    }

    class Impl(userRepo: UserRepository.Service) extends Service{
        val dc = db.Ctx
        import dc._

        def listUsers(): RIO[db.DataSource, List[User]] =
        userRepo.list()


        def listUsersDTO(): RIO[db.DataSource,List[UserDTO]] = for {
            users <- listUsers()
            usersWithRoles <- RIO.collectAll(users.map(u=>userRepo.userRoles(u.typedId).map(l=> u->l)))
            result <- RIO.succeed(usersWithRoles.map(u=>UserDTO(u._1, u._2.toSet)))
        } yield result

        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = {
         dc.transaction(
            for {
            users<-userRepo.createUser(user)
            _<-userRepo.insertRoleToUser(roleCode, user.typedId)
            roles <- userRepo.userRoles(users.typedId)
            result <- RIO.succeed(UserDTO(users, roles.toSet))
           } yield result
         )
        }

        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource,List[UserDTO]] = for {
            users<-userRepo.listUsersWithRole(roleCode)
            usersWithRoles <- RIO.collectAll(users.map(u=>userRepo.userRoles(u.typedId).map(l=> u->l)))
            result <- RIO.succeed(usersWithRoles.map(u=>UserDTO(u._1, u._2.toSet)))
        } yield result
        
        
    }

    val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] = ZLayer.fromService[UserRepository.Service, UserService.Service](repo=>new Impl(repo))
}

case class UserDTO(user: User, roles: Set[Role])

