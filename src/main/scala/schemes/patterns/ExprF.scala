package schemes

package patterns

import cats.{Functor, PartialOrder}
import cats.syntax.partialOrder._
import cats.derived

sealed trait ExprF[+A] extends Product with Serializable

object ExprF {

  final case class Lit(x: Int) extends ExprF[Nothing]
  final case class Add[A](x: A, y: A) extends ExprF[A]
  final case class Sub[A](x: A, y: A) extends ExprF[A]
  final case class Mult[A](x: A, y: A) extends ExprF[A]
  final case class Div[A](x: A, y: A) extends ExprF[A]

  implicit val functor: Functor[ExprF] =
    derived.semi.functor

  implicit def partialOrder[A]: PartialOrder[ExprF[A]] =
    PartialOrder.from[ExprF[A]] {
      case (Add(_, _), Mult(_, _)) => -1.0
      case (Add(_, _), Div(_, _))  => -1.0
      case (Sub(_, _), Mult(_, _)) => -1.0
      case (Sub(_, _), Div(_, _))  => -1.0
      case _                       => Double.NaN
    }

  type Expr = Fix[ExprF]

  def lit(x: Int): Expr = Fix[ExprF](Lit(x))
  def add(x: Expr, y: Expr): Expr = Fix(Add(x, y))
  def sub(x: Expr, y: Expr): Expr = Fix(Sub(x, y))
  def mult(x: Expr, y: Expr): Expr = Fix(Mult(x, y))
  def div(x: Expr, y: Expr): Expr = Fix(Div(x, y))

  val eval: Algebra[ExprF, Int] = {
    case Lit(x)     => x
    case Add(x, y)  => x + y
    case Sub(x, y)  => x - y
    case Mult(x, y) => x * y
    case Div(x, y)  => x / y
  }

  val show: Algebra[ExprF, String] = {
    case Lit(x)     => x.toString
    case Add(x, y)  => s"($x + $y)"
    case Sub(x, y)  => s"($x - $y)"
    case Mult(x, y) => s"($x * $y)"
    case Div(x, y)  => s"($x / $y)"
  }

  val showRpn: Algebra[ExprF, String] = {
    case Lit(x)     => x.toString
    case Add(x, y)  => s"$x $y +"
    case Sub(x, y)  => s"$x $y -"
    case Mult(x, y) => s"$x $y *"
    case Div(x, y)  => s"$x $y /"
  }

  def withParens[A](parent: ExprF[A], childExpr: ExprF[A], childStr: String) =
    if (childExpr < parent) s"($childStr)" else childStr

  val minimalParens: RAlgebra[ExprF, Expr, String] = {
    case Lit(x)                             => x.toString
    case Add((_, x), (_, y))                => s"$x + $y"
    case Sub((_, x), (_, y))                => s"$x - $y"
    case e @ Mult((Fix(l), x), (Fix(r), y)) => s"${withParens(e, l, x)} * ${withParens(e, r, y)}"
    case e @ Div((Fix(l), x), (Fix(r), y))  => s"${withParens(e, l, x)} / ${withParens(e, r, y)}"
  }
}

object ExprExample extends App {

  import ExprF._

  val expr =
    mult(
      add(lit(1), lit(1)),
      add(
        mult(lit(2), lit(5)),
        sub(lit(10), lit(50))
      )
    )

  val (exprString, result) = expr.cata(show zip eval)
  println(s"$exprString = $result")

  // Output:
  // ((1 + 1) * ((2 * 5) + (10 - 50))) = -60

  println(expr.cata(showRpn))

  // Output:
  // 1 1 + 2 5 * 10 50 - + *

  println(expr.para(minimalParens))

  // Output:
  // (1 + 1) * (2 * 5 + 10 - 50)
}
