import cats.Functor

package object schemes {

  type Algebra[F[_], A] = F[A] => A

  def cata[F[_], A](fixed: Fix[F])(alg: Algebra[F, A])(implicit F: Functor[F]): A =
    alg(F.map(fixed.unfix)(cata(_)(alg)))
}
