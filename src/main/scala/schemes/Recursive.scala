package schemes

import cats.Functor
import cats.syntax.functor._

trait Recursive[T] extends Base {

  def project(t: T): Base[T]

  final def cata[A](t: T)(alg: Algebra[Base, A]): A =
    alg(project(t).map(cata(_)(alg)))
}

object Recursive {

  type Aux[T, F[_]] = Recursive[T] { type Base[A] = F[A] }

  def instance[T, F[_]](proj: T => F[T])(implicit F: Functor[F]): Recursive.Aux[T, F] =
    new Recursive[T] {
      type Base[A] = F[A]
      implicit val baseFunctor: Functor[F] = F
      def project(t: T): F[T] = proj(t)
    }
}
