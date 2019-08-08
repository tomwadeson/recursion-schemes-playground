package schemes

import cats.Functor

trait Recursive[T] {

  type Base[A]
  implicit val BF: Functor[Base]

  def project(t: T): Base[T]

  final def cata[A](t: T)(alg: Algebra[Base, A]): A =
    alg(BF.map(project(t))(cata(_)(alg)))
}

object Recursive {

  type Aux[T, F[_]] = Recursive[T] { type Base[A] = F[A] }

  def apply[T](implicit ev: Recursive[T]): Recursive.Aux[T, ev.Base] = ev

  object syntax {

    implicit class RecursiveOps[T, F[_]](target: T)(implicit ev: Aux[T, F]) {

      def project: F[T] =
        ev.project(target)

      def cata[A](alg: Algebra[F, A]): A =
        ev.cata[A](target)(alg)
    }
  }
}
