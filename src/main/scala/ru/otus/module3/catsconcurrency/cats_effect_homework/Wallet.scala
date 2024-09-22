package ru.otus.module3.catsconcurrency.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import Wallet._

import java.nio.file.Files._
import java.nio.file.Paths._
import java.nio.file.{Path, StandardOpenOption}

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {

  private def fileName(id:String):String=s"${id}.txt"

  private def walletPath(id:WalletId):F[Path]=Sync[F].delay(java.nio.file.Paths.get(fileName(id)))

  def balance: F[BigDecimal] = for {
    path <- walletPath(id)
    fileExists <- Sync[F].delay(java.nio.file.Files.exists(path))
    balanceStr <- Sync[F].delay(if(fileExists) java.nio.file.Files.readAllLines(path).get(0) else "0")
    res <- Sync[F].delay(BigDecimal(balanceStr))
  } yield res

  def topup(amount: BigDecimal): F[Unit] = for {
    currentBalance <- balance
    path <- walletPath(id)
    _ <- Sync[F].delay(java.nio.file.Files.write(path, (currentBalance+amount).toString().getBytes()))

  } yield ()

  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = for {
    currentBalance <- balance
    path <- walletPath(id)
    res <- Sync[F].delay(
             if(currentBalance-amount<0)
                Left(BalanceTooLow)
             else {
               java.nio.file.Files.write(path, (currentBalance-amount).toString().getBytes())
               Right(())
             }

         )

  } yield res

}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] = Sync[F].delay(new FileWallet[F](id))

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}
