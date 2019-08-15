package schemes

package patterns

import cats.Functor
import cats.derived

sealed trait PeanoF[+A]

object PeanoF {

  type Peano = Fix[PeanoF]

  final case class Succ[A](n: A) extends PeanoF[A]
  final case object Zero extends PeanoF[Nothing]

  implicit val functor: Functor[PeanoF] =
    derived.semi.functor

  val toInt: Algebra[PeanoF, Int] = {
    case Succ(n) => n + 1
    case Zero    => 0
  }

  val fromInt: Coalgebra[PeanoF, Int] =
    n => if (n <= 0) Zero else Succ(n - 1)

}

object PeanoExample extends App {

  import PeanoF._

  val _3p = 3.ana[Peano](fromInt)
  val _3i = _3p.cata(toInt)

  println(_3p)
  println(_3i)

  // Output:
  // Fix(Succ(Fix(Succ(Fix(Succ(Fix(Zero)))))))
  // 3

  import cats.implicits._
  type Peano2 = Fix[Option]

  val fromInt2: Coalgebra[Option, Int] =
    n => if (n <= 0) None else Some(n - 1)

  val toInt2: Algebra[Option, Int] = {
    case Some(n) => n + 1
    case None    => 0
  }

  val _3p2 = 3.ana[Peano2](fromInt2)
  val _3i2 = _3p2.cata(toInt2)

  println(_3p2)
  println(_3i2)

  // Output:
  // Fix(Some(Fix(Some(Fix(Some(Fix(None)))))))
  // 3
}
