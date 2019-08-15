package schemes

import cats.Functor

trait Base {
  type Base[A]
  implicit val BF: Functor[Base]
}
