package schemes

import cats.Functor
import schemes.patterns.ListF

trait Birecursive[T] extends Recursive[T] with Corecursive[T]

object Birecursive {

  type Aux[T, F[_]] = Birecursive[T] { type Base[A] = F[A] }

  def instance[T, F[_]](proj: T => F[T], em: F[T] => T)(implicit F: Functor[F]): Birecursive.Aux[T, F] =
    new Birecursive[T] {
      type Base[A] = F[A]
      implicit val baseFunctor: Functor[F] = F

      override def project(t: T): F[T] = proj(t)

      override def embed(bt: F[T]): T = em(bt)
    }

  implicit def scalaList[A]: Aux[List[A], ListF[A, ?]] =
    instance[List[A], ListF[A, ?]]({
      case head :: tail => ListF.Cons(head, tail)
      case Nil          => ListF.Nil
    }, {
      case ListF.Cons(head, tail) => head :: tail
      case ListF.Nil              => Nil
    })
}
