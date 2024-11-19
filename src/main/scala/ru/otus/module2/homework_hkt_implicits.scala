package ru.otus.module2

object homework_hkt_implicits{

  trait Bindable[F[_], A]{
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  def tuplef[F[_], A, B](fa: F[A], fb: F[B])(implicit ca:F[A]=>Bindable[F,A], cb:F[B]=>Bindable[F,B]): F[(A, B)] = {

    ca(fa).flatMap(a => cb(fb).map(b => (a, b)))
  }

  implicit def listBindable[A](list:List[A]):Bindable[List,A]= new Bindable[List,A] {
       override def map[B](f:A=>B):List[B]=list.map(f)
       override def flatMap[B](fa:A=>List[B]):List[B]=list.flatMap(fa)
    }
  implicit def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
    override def map[B](fb: A => B): Option[B] = opt.map(fb)
    override def flatMap[B](fb: A => Option[B]): Option[B] = opt.flatMap(fb)
  }

  val l1:List[Int]=List(1,2,3)
  val l2:List[Char]=List('a','b','c')

  val tupleList=tuplef[List,Int,Char](l1,l2)

  val o1:Option[Int]=Some(1)
  val o2:Option[Char]=Some('a')

  val tupleOption=tuplef[Option,Int,Char](o1,o2)

}