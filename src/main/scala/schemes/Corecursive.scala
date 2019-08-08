package schemes

import cats.Functor
import cats.syntax.functor._
import schemes.patterns.ListF

trait Corecursive[T] {

  type Base[A]
  implicit val baseFunctor: Functor[Base]

  def embed(bt: Base[T]): T

  final def ana[A](a: A)(coalg: Coalgebra[Base, A]): T =
    embed(coalg(a).map(ana(_)(coalg)))
}

object Corecursive {

  type Aux[T, F[_]] = Corecursive[T] { type Base[A] = F[A] }

  def instance[T, F[_]](em: F[T] => T)(implicit F: Functor[F]): Corecursive.Aux[T, F] =
    new Corecursive[T] {
      type Base[A] = F[A]
      implicit val baseFunctor: Functor[F] = F
      override def embed(bt: F[T]): T = em(bt)
    }

  implicit def scalaList[A]: Aux[List[A], ListF[A, ?]] =
    instance[List[A], ListF[A, ?]] {
      case ListF.Cons(head, tail) => head :: tail
      case ListF.Nil              => List.empty
    }
}
