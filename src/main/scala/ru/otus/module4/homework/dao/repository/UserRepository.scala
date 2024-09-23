package ru.otus.module4.homework.dao.repository

import zio.{Has, ULayer, ZIO, ZLayer}
import io.getquill.context.ZioJdbc._
import ru.otus.module4.homework.dao.entity._
import ru.otus.module4.phoneBook.db

import java.sql.SQLException
import javax.sql.DataSource


object UserRepository{


    val dc = db.Ctx
    import dc._

    type UserRepository = Has[Service]

    trait Service{
        def findUser(userId: UserId): QIO[Option[User]]
        def createUser(user: User): QIO[User]
        def createUsers(users: List[User]): QIO[List[User]]
        def updateUser(user: User): QIO[Unit]
        def deleteUser(user: User): QIO[Unit]
        def findByLastName(lastName: String): QIO[List[User]]
        def list(): QIO[List[User]]
        def userRoles(userId: UserId): QIO[List[Role]]
        def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]
        def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]
        def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
    }

    class ServiceImpl extends Service{

        val userSchema = quote {
            querySchema[User]("""users""")
        }

        val userToRoleSchema = quote {
            querySchema[UserToRole]("""usertorole""")
        }

        val roleSchema = quote {
            querySchema[Role]("""roles""")
        }

        override def findUser(userId: UserId): QIO[Option[User]] = dc.run(userSchema.filter(_.id == lift(userId.id)).sortBy(_.id).take(1)).map(_.headOption)

        override def createUser(user: User): QIO[User] = dc.run(userSchema.insert(lift(user)).returning[User]((userRecord:User)=>userRecord))

        override def createUsers(users: List[User]): QIO[List[User]] = dc.run(liftQuery(users).foreach { ur=>userSchema.insert(ur).returning[User]((userRecord:User)=>userRecord) } )

        override def updateUser(user: User): QIO[Unit] = dc.run(userSchema.filter(_.id == lift(user.id)).update(lift(user))).unit

        override def deleteUser(user: User): QIO[Unit] = dc.run(userSchema.filter(_.id == lift(user.id)).delete).unit

        override def findByLastName(lastName: String): QIO[List[User]] = dc.run(userSchema.filter(_.lastName==lift(lastName)))

        override def list(): QIO[List[User]] = dc.run(userSchema)

        override def userRoles(userId: UserId): QIO[List[Role]] = dc.run(
            for
             {userRecord<-userSchema.filter(_.id==lift(userId.id))
              userToRoleRecord<-userToRoleSchema.join(_.userId==userRecord.id)
              roleRecord<-roleSchema.join(_.code==userToRoleRecord.roleId)
              } yield (roleRecord) )

        override def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit] = dc.run(userToRoleSchema.insert(lift(UserToRole(roleCode.code, userId.id )))).unit

        override def listUsersWithRole(roleCode: RoleCode): QIO[List[User]] = dc.run(
            for {
                roleRecord<-roleSchema.filter(_.code==lift(roleCode.code))
                userToRoleRecord<-userToRoleSchema.join(_.roleId==roleRecord.code)
                userRecord<-userSchema.join(_.id==userToRoleRecord.userId)
            } yield (userRecord)
        )

        override def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]] = dc.run(roleSchema.filter(_.code==lift(roleCode.code)).sortBy(_.code).take(1)).map(_.headOption)
    }

    val live: ULayer[UserRepository] = ZLayer.succeed(new ServiceImpl)
}