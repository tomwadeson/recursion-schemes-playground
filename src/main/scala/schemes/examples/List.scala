package schemes

package examples

import cats.Functor

sealed trait ListF[+A, +B] extends Product with Serializable

object ListF {

  type List[A] = Fix[ListF[A, ?]]

  final case class Cons[A, B](head: A, tail: B) extends ListF[A, B]

  final case object Nil extends ListF[Nothing, Nothing]

  def cons[A](a: A, list: List[A]): List[A] =
    Fix[ListF[A, ?]](ListF.Cons(a, list))

  def nil[A]: List[A] =
    Fix[ListF[A, ?]](ListF.Nil)

  implicit def functorB[X]: Functor[ListF[X, ?]] = new Functor[ListF[X, ?]] {
    override def map[A, B](fa: ListF[X, A])(f: A => B): ListF[X, B] =
      fa match {
        case Cons(head, tail) => Cons(head, f(tail))
        case Nil              => Nil
      }
  }

  object algebras {

    def algebra[A, B](base: B)(f: (A, B) => B): Algebra[ListF[A, ?], B] = {
      case ListF.Cons(head, tail) => f(head, tail)
      case ListF.Nil              => base
    }

    def sum[A](implicit N: Numeric[A]): Algebra[ListF[A, ?], A] =
      algebra(N.zero)(N.plus)

    def product[A](implicit N: Numeric[A]): Algebra[ListF[A, ?], A] =
      algebra(N.one)(N.times)

    def size[A]: Algebra[ListF[A, ?], Int] =
      algebra(0)((_, acc) => 1 + acc)

    def map[A, B](f: A => B): Algebra[ListF[A, ?], List[B]] =
      algebra(nil[B])((x, acc) => cons(f(x), acc))
  }
}

object ListExample extends App {

  import ListF._
  import ListF.algebras._

  val list: List[Int] =
    cons(1, cons(2, cons(3, cons(4, cons(5, nil)))))

  println(cata(list)(sum))
  println(cata(list)(product))
  println(cata(list)(size))
  println(cata(list)(map(_ * 10)))

  // Output:
  // 15
  // 120
  // 5
  // Fix(Cons(10,Fix(Cons(20,Fix(Cons(30,Fix(Cons(40,Fix(Cons(50,Fix(Nil)))))))))))

  import Recursive.syntax._

  val x: Recursive[List[Int]] = Recursive[List[Int]]
  val y: Recursive.Aux[List[Int], ListF[Int, ?]] = Recursive[List[Int]]

  println(list.cata(sum))
  println(list.cata(product))
  println(list.cata(size))
  println(list.cata(map(_ * 10)))

  // Output:
  // 15
  // 120
  // 5
  // Fix(Cons(10,Fix(Cons(20,Fix(Cons(30,Fix(Cons(40,Fix(Cons(50,Fix(Nil)))))))))))

}
