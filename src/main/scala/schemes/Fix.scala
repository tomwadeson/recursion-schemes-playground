package schemes

import cats.Functor

final case class Fix[F[_]](unfix: F[Fix[F]])

object Fix {

  implicit def recursive[F[_]: Functor]: Recursive.Aux[Fix[F], F] =
    Recursive.instance[Fix[F], F](_.unfix)

  implicit def corecursive[F[_]: Functor]: Corecursive.Aux[Fix[F], F] =
    Corecursive.instance[Fix[F], F](Fix(_))
}
