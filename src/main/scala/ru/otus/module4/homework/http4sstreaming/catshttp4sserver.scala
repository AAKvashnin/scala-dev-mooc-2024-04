package ru.otus.module4.homework.http4sstreaming


import cats.effect.{IO, IOApp, Resource}
import org.http4s.{Http, HttpRoutes, Status}
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import com.comcast.ip4s.{Host, Port}
import cats.effect.kernel.Ref
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.http4s.server.Router
import fs2.Stream
import scala.concurrent.duration._



object Restfull {

  case class CounterJSon(counter:Int)

  implicit val fooEncoder: Encoder[CounterJSon] = deriveEncoder[CounterJSon]

  type Counter[F[_]] = Ref[F, Int]
  def serviceCounter(counter:Counter[IO]): HttpRoutes[IO] = HttpRoutes.of{
    case GET -> Root / "counter"  => counter.update(_ + 1).flatMap(_=>counter.get).flatMap(x=>Ok(CounterJSon(x).asJson.toString()))
  }

  def drip(chunk:Int,total:Int,time:Int):Stream[IO,String] = {
    Stream.awakeEvery[IO](time*1000.millis).map(_.toString).take(total/chunk)
  }


  def serviceSlow:HttpRoutes[IO]=HttpRoutes.of{
    case GET -> Root / "slow" / chunk / total / time => Ok(drip(chunk.toInt, total.toInt, time.toInt))
  }

  def router(counter: Counter[IO]) = Router(
    "/" -> serviceCounter(counter),
    "/counter"-> serviceCounter(counter),
    "/slow" -> serviceSlow
  )


  def httpApp(counter:Counter[IO]): Http[IO, IO] = router(counter).orNotFound

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