package schemes

import cats.Functor

trait Recursive[T] {

  type Base[A]

  def project(t: T)(implicit F: Functor[Base]): Base[T]

  final def cata[A](t: T)(alg: Algebra[Base, A])(implicit F: Functor[Base]): A =
    alg(F.map(project(t))(cata(_)(alg)))
}

object Recursive {

  type Aux[T, F[_]] = Recursive[T] { type Base[A] = F[A] }

  def apply[T](implicit ev: Recursive[T]): Recursive.Aux[T, ev.Base] = ev

  object syntax {

    implicit class RecursiveOps[T, F[_]](target: T)(implicit ev: Aux[T, F]) {

      def project(implicit F: Functor[F]): F[T] =
        ev.project(target)

      def cata[A](alg: Algebra[F, A])(implicit F: Functor[F]): A =
        ev.cata[A](target)(alg)
    }
  }
}
