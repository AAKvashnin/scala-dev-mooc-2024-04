package ru.otus.module4.homework.http4sstreaming

import org.http4s.{Method, Uri}
import org.scalatest.flatspec.AnyFlatSpec
import cats.effect.{IO, Ref, Resource}
import org.http4s.{Method, Request, Response}
import cats.effect.unsafe.implicits.global
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.Status.BadRequest
class SlowServiceSpec extends AnyFlatSpec {

  "check status success" should "ok" in {
    val result = for {
      response <- Ref.of[IO, Int](1).flatMap {
        cnt => Restfull.router(cnt).run(Request(Method.GET, uri"/slow/10/50/5")).value
      }
      isSuccess <- IO.pure(response.get.status.isSuccess)
    } yield (isSuccess)

    assert(result.unsafeRunSync() === true)
  }

  "check status letter" should "ok" in  {

    val result = for {
      response <- Ref.of[IO, Int](1).flatMap {
        cnt => Restfull.router(cnt).run(Request(Method.GET, uri"/slow/10/50/g")).value.flatMap(r=>IO.pure(r))
      }
      status <- IO.pure(response.get.status)
    } yield (status)

    assert(result.unsafeRunSync() === BadRequest)

  }

  "check status negative" should "ok" in {

    val result = for {
      response <- Ref.of[IO, Int](1).flatMap {
        cnt => Restfull.router(cnt).run(Request(Method.GET, uri"/slow/10/50/-1")).value.flatMap(r => IO.pure(r))
      }
      status <- IO.pure(response.get.status)
    } yield (status)

    assert(result.unsafeRunSync() === BadRequest)

  }

}