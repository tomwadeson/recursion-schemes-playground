package schemes

final case class Fix[F[_]](unfix: F[Fix[F]])

