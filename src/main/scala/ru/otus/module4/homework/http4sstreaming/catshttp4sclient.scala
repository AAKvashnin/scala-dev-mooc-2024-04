package ru.otus.module4.homework.http4sstreaming

import org.http4s.ember.client.EmberClientBuilder
import cats.effect.{IO,IOApp,Resource}
import org.http4s.client.Client
import org.http4s.{Request, Response, Uri}
import cats.effect

