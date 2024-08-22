package ru.otus.module3

import scala.language.postfixOps
import zio.console.{Console, putStrLn}
import zio.random.Random
import zio.{Has, IO, ULayer, ZIO, ZLayer}
import zio.config.ReadError
import zio.clock.{Clock, sleep}
import zio.duration.durationInt

import scala.util.Try
import ru.otus.module3.zio_homework.config.{AppConfig, load}

import java.io.File
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.typesafe.TypesafeConfigSource
import ru.otus.module3.zioConcurrency.{currentTime, printEffectRunningTime}
import ru.otus.module3.printEffectiveRunningTimeService.PrintEffRunTimeService.Service
import ru.otus.module3.printEffectiveRunningTimeService.PrintEffRunTimeService
package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  lazy val getString:ZIO[Console, Throwable, String] = {
    for {
      console <- ZIO.environment[Console].map(_.get)
      input<-console.getStrLn
    } yield (input)
  }

  def putString(str:String):ZIO[Console, Throwable, Unit] = {
    for {
      console <- ZIO.environment[Console].map(_.get)
      _ <- console.putStrLn(str).orDie
    } yield ()
  }

  lazy val getInt:ZIO[Console, Throwable, Int] = {
    getString.flatMap(str=>ZIO.fromTry(Try(str.toInt)))
  }

  lazy val readIntOrRetry:ZIO[Console,Throwable,Int] = {
     getInt.orElse(putString("Некорректный ввод") zipRight readIntOrRetry)
  }


  lazy val guessProgram : ZIO[Random with Console, Throwable, Unit] = {
    for {
      random <- ZIO.environment[Random].map(_.get)
      rand <- random.nextIntBetween(1, 4)
      _ <- putString("Input integer from 1 to 3")
      input <- readIntOrRetry
      _ <- putString((if (rand == input) "Right" else "Wrong") + " guess. Your input: " + input + ", random  is: " + rand)
    } yield ()
  }

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R,E,A](zio:ZIO[R,E,A])(f:A=>Boolean):ZIO[R,E,A] = {

    zio.flatMap(a=>if (f(a)) doWhile(zio)(f) else ZIO.succeed(a) )
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault(fileName:String):ZIO[Console, ReadError[String], AppConfig] =
  {
    TypesafeConfigSource.fromHoconFile(new File(fileName)) match {
      case Left(_) => { val r=load; putStrLn(r.toString); r }
      case Right(source) => zio.config.read( descriptor[AppConfig].from(source)) match {
        case Left(_) => { val r=load; putStrLn(r.toString); r }
        case Right(s)=>ZIO.succeed(s)
      }
    }
  }





  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff:ZIO[Random with Clock with Console,Throwable,Int]= {
    for {
      random <- ZIO.environment[Random].map(_.get)
      clock <- ZIO.environment[Clock].map(_.get)
      _ <- clock.sleep(1 seconds)
      rand <- random.nextIntBetween(0, 11)
    } yield(rand)
  }

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects:List[ZIO[Random with Clock with Console,Throwable,Int]] = List.fill(10)(eff)

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app:ZIO[Random with Clock with Console,Throwable,Int] = {
       printEffectRunningTime(ZIO.collectAll(effects).foldM(
         error => ZIO.fail(error),
         success => ZIO.effect( {success.sum} ).flatMap(i=>ZIO.succeed(i) zipLeft putString("Sum is "+i.toString))
         )
       )
    }



  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp:ZIO[Random with Clock with Console, Throwable, Int] = {
    printEffectRunningTime(ZIO.collectAllPar(effects).foldM(
      error => ZIO.fail(error),
      success => ZIO.effect( { success.sum} ).flatMap(i => ZIO.succeed(i) zipLeft putString("Sum is " + i.toString))
    )
    )
  }


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.console.putStrLn например
   */
   // See below




   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg:ZIO[Random with Clock with Console with PrintEffRunTimeService,Throwable,Int] = {
      PrintEffRunTimeService.printEffectRunnTime(app )
  }

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  val env=PrintEffRunTimeService.live

  lazy val runApp = appWithTimeLogg.provideSomeLayer[Clock with Console with Random]

}


package object printEffectiveRunningTimeService {

 type PrintEffRunTimeService = Has[Service]

  object PrintEffRunTimeService {

  trait Service {
    def printEffectRunnTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A]
  }


  val live: ZLayer[Clock with Console, Nothing, PrintEffRunTimeService] =
    ZLayer.succeed(new Service {
      override def printEffectRunnTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A] = {
        for {
          start <- currentTime
          r <- zio
          end <- currentTime
          _ <- putStrLn(s"Running time ${end - start}").orDie
        } yield r
      }
    })

      def printEffectRunnTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with PrintEffRunTimeService with Clock with Console, E, A] =
        ZIO.accessM(_.get.printEffectRunnTime(zio))


    }
  }

