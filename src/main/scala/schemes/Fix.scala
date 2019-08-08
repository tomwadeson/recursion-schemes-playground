package schemes

import cats.Functor

final case class Fix[F[_]](unfix: F[Fix[F]])

object Fix {

  implicit def recursive[F[_]: Functor]: Recursive.Aux[Fix[F], F] =
    Recursive.instance[Fix[F], F](_.unfix)
}
