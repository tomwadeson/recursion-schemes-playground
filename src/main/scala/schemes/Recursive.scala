package schemes

import cats.Functor
import cats.implicits._

trait Recursive[T] extends Base {

  def project(t: T): Base[T]

  def cata[A](t: T)(alg: Algebra[Base, A]): A =
    hylo(alg, project)(t)

  def para[A](t: T)(rAlg: RAlgebra[Base, T, A]): A =
    // rAlg(project(t).map(t => (t, para(t)(rAlg))))
    hylo[Lambda[X => Base[(T, X)]], T, A](rAlg, t => project(t).tupleLeft(t))(t)(BF.compose[(T, ?)])
}

object Recursive {

  type Aux[T, F[_]] = Recursive[T] { type Base[A] = F[A] }

  def instance[T, F[_]](proj: T => F[T])(implicit F: Functor[F]): Recursive.Aux[T, F] =
    new Recursive[T] {
      type Base[A] = F[A]
      implicit val BF: Functor[F] = F
      def project(t: T): F[T] = proj(t)
    }
}
