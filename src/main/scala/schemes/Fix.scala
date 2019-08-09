package schemes

import cats.Functor

final case class Fix[F[_]](unfix: F[Fix[F]])

object Fix {

  implicit def birecursive[F[_]: Functor]: Birecursive.Aux[Fix[F], F] =
    Birecursive.instance[Fix[F], F](_.unfix, Fix(_))
}
