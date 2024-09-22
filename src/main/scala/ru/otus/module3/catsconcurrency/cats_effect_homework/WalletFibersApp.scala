package ru.otus.module3.catsconcurrency.cats_effect_homework

import cats.effect.{IO, IOApp}
import cats.implicits._
import cats.effect.unsafe.implicits.global



// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def IncrementAmount(wallet:Wallet[IO], amount:BigDecimal, timeout:Long):IO[Unit] =
    wallet.topup(amount).flatMap(_=>IO.delay(Thread.sleep(timeout))).flatMap(_=>IncrementAmount(wallet,amount,timeout))

  def PrintWallets(wallets:List[Wallet[IO]]):IO[Unit] = {
    IO.delay(wallets.zipWithIndex.foreach(w=>println("Wallet "+w._2+" has "+w._1.balance.unsafeRunSync())) ) .flatMap(_=>IO.delay(Thread.sleep(1000))).flatMap(_=>PrintWallets(wallets))
  }


  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      _ <- IncrementAmount(wallet1,100,200).start
      _ <- IncrementAmount(wallet2, 100, 500).start
      _ <- IncrementAmount(wallet3, 100, 2000).start
      _ <- PrintWallets(List(wallet1, wallet2, wallet3)).start
      _ <- IO.readLine
      // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
    } yield ()

}
