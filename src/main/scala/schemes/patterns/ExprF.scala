package schemes

package patterns

import cats.Functor

sealed trait ExprF[+A] extends Product with Serializable

object ExprF {

  final case class Lit(x: Int) extends ExprF[Nothing]
  final case class Add[A](x: A, y: A) extends ExprF[A]
  final case class Sub[A](x: A, y: A) extends ExprF[A]
  final case class Mult[A](x: A, y: A) extends ExprF[A]
  final case class Div[A](x: A, y: A) extends ExprF[A]

  implicit val functor: Functor[ExprF] = new Functor[ExprF] {
    override def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
      case Lit(x)     => Lit(x)
      case Add(x, y)  => Add(f(x), f(y))
      case Sub(x, y)  => Sub(f(x), f(y))
      case Mult(x, y) => Mult(f(x), f(y))
      case Div(x, y)  => Div(f(x), f(y))
    }
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

  println(expr.cata(eval))
  println(expr.cata(show))

}
