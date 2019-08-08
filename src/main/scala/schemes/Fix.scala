package schemes

import cats.Functor

final case class Fix[F[_]](unfix: F[Fix[F]])

object Fix {

  implicit def recursive[F[_]]: Recursive.Aux[Fix[F], F] =
    new Recursive[Fix[F]] {
      type Base[A] = F[A]

      def project(t: Fix[F])(implicit F: Functor[F]): F[Fix[F]] =
        t.unfix
    }
}
