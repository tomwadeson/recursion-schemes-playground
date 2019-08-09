package schemes

import cats.Functor

trait Base {
  type Base[A]
  implicit val baseFunctor: Functor[Base]
}
