package schemes

import cats.Functor

final case class Fix[F[_]](unfix: F[Fix[F]])

object Fix {

  implicit def recursive[F[_]](implicit F: Functor[F]): Recursive.Aux[Fix[F], F] =
    new Recursive[Fix[F]] {
      type Base[A] = F[A]
      implicit val BF: Functor[F] = F
      def project(t: Fix[F]): F[Fix[F]] = t.unfix
    }
}
