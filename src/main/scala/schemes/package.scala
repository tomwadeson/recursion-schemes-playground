package object schemes {

  type Algebra[F[_], A] = F[A] => A

  type Coalgebra[F[_], A] = A => F[A]

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
  }
}
