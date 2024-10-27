package ru.otus.module4.homework.http4sstreaming

import org.http4s.ember.client.EmberClientBuilder
import cats.effect.{IO, IOApp, Resource}
import org.http4s.client.Client
import org.http4s.{Method, Request, Response, Uri}
import cats.effect

object HttpClient {

  val builder = EmberClientBuilder.default[IO].build

  val request = Request[IO](
    method = Method.GET, Uri.fromString("http://localhost:8080").toOption.get
  )

  val result = for {
    client <- builder
    response <- client.run(request)
  } yield response

  val printResponse = result.use(IO.println)

}
