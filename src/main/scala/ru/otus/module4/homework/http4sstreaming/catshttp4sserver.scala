package ru.otus.module4.homework.http4sstreaming

import cats.Functor
import cats.data.Kleisli
import cats.effect.{IO, IOApp, Resource}
import org.http4s.{Http, HttpRoutes, Status}
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import com.comcast.ip4s.{Host, Port}
import cats.effect.kernel.Ref

object Restfull {

  type Counter[F[_]] = Ref[F, Int]
  def service(counter:Counter[IO]): HttpRoutes[IO] = HttpRoutes.of{
    case GET -> Root / "counter"  => counter.update(_ + 1).flatMap(_=>counter.get).flatMap(x=>Ok(x.toString))

  }


  def httpApp(counter:Counter[IO]): Http[IO, IO] = service(counter).orNotFound

  val server1 = for {
    counter<- Resource.eval(Ref.of[IO, Int](0))
    s<-EmberServerBuilder
      .default[IO]
      .withHost(Host.fromString("localhost").get)
      .withPort(Port.fromInt(8080).get)
      .withHttpApp(httpApp(counter)).build
  } yield s



}

object mainServer extends IOApp.Simple {
  def run(): IO[Unit] = {
    Restfull.server1.use( _ => IO.never)
  }
}