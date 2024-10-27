package ru.otus.module4.homework.http4sstreaming

import org.http4s.{Method, Uri}
import org.scalatest.flatspec.AnyFlatSpec
import cats.effect.{IO, Ref, Resource}
import org.http4s.{Method, Request, Response}
import cats.effect.unsafe.implicits.global
import org.http4s.implicits.http4sLiteralsSyntax
import Restfull.CounterJSon
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.http4s.circe.CirceEntityDecoder._




class CounterServiceSpec extends AnyFlatSpec {

  "check status success" should "ok" in {
     val result = for {
       response <- Ref.of[IO,Int](1).flatMap {
         cnt=>Restfull.router(cnt).run(Request(Method.GET, uri"/counter")).value}
       isSuccess <- IO.pure(response.get.status.isSuccess)
     } yield (isSuccess)

    assert(result.unsafeRunSync()===true)
  }

  "check next counter" should "ok" in {
    implicit val fooDecoder: Decoder[CounterJSon] = deriveDecoder[CounterJSon]

    val result = for {
      response <- Ref.of[IO, Int](1).flatMap {
        cnt => Restfull.router(cnt).run(Request(Method.GET, uri"/counter")).value
      }
      next <- response.get.as[CounterJSon]
    } yield (next)

    assert(result.unsafeRunSync() === CounterJSon(2))
  }

}