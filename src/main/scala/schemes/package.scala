import cats.Functor
import cats.syntax.functor._

package object schemes {

  type Algebra[-F[_], A] = F[A] => A

  type Coalgebra[F[_], A] = A => F[A]

  type RAlgebra[F[_], T, A] = F[(T, A)] => A

  def hylo[F[_]: Functor, A, B](alg: Algebra[F, B], coalg: Coalgebra[F, A])(a: A): B =
    alg(coalg(a).map(hylo(alg, coalg)(_)))

  implicit def birecursiveIsRecursive[T, F[_]](
      implicit ev: Birecursive.Aux[T, F]
  ): Recursive.Aux[T, F] = ev

  implicit def birecursiveIsCorecursive[T, F[_]](
      implicit ev: Birecursive.Aux[T, F]
  ): schemes.Corecursive.Aux[T, F] = ev

  implicit class IdOps[A](target: A) {

    object ana {
      def apply[T] = new PartiallyApplied[T]

      final class PartiallyApplied[T] {

        def apply[F[_]](coalg: Coalgebra[F, A])(implicit ev: Corecursive.Aux[T, F]): T =
          ev.ana(target)(coalg)
      }
    }
  }

  implicit class RecursiveOps[T, F[_]](target: T)(implicit ev: Recursive.Aux[T, F]) {

    def project: F[T] =
      ev.project(target)

    def cata[A](alg: Algebra[F, A]): A =
      ev.cata[A](target)(alg)

    def para[A](rAlg: RAlgebra[F, T, A]): A =
      ev.para(target)(rAlg)
  }

  implicit class AlgebraOps[F[_]: Functor, A](algA: Algebra[F, A]) {

    def zip[B](algB: Algebra[F, B]): Algebra[F, (A, B)] =
      fab => {
        val a = algA(fab.map(_._1))
        val b = algB(fab.map(_._2))
        (a, b)
      }
  }
}
