package schemes

package patterns

import cats.Functor
import cats.data.Nested
import cats.implicits._
import cats.derived

sealed trait ListF[+A, +B] extends Product with Serializable

object ListF {

  type MyList[A] = Fix[ListF[A, ?]]

  final case class Cons[A, B](head: A, tail: B) extends ListF[A, B]

  final case object Nil extends ListF[Nothing, Nothing]

  def cons[A](a: A, list: MyList[A]): MyList[A] =
    Fix[ListF[A, ?]](ListF.Cons(a, list))

  def nil[A]: MyList[A] =
    Fix[ListF[A, ?]](ListF.Nil)

  implicit def functor[A]: Functor[ListF[A, ?]] =
    derived.semi.functor

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

    def map[A, B](f: A => B): Algebra[ListF[A, ?], MyList[B]] =
      algebra(nil[B])((x, acc) => cons(f(x), acc))
  }

  object coalgebras {

    val range: Coalgebra[ListF[Int, ?], Int] =
      n => if (n > 0) ListF.Cons(n, n - 1) else ListF.Nil
  }

  object compose {

    object list {

      type ListF[A, B] = Nested[Option, (A, ?), B]
      type List[A] = Fix[ListF[A, ?]]

      def cons[A](head: A, tail: List[A]): List[A] =
        Fix(Nested(Some((head, tail))))

      def nil[A]: List[A] = Fix(Nested(None))

      def product[A](implicit N: Numeric[A]): Algebra[ListF[A, ?], A] =
        fa =>
          fa.value match {
            case Some((head, tail)) => N.times(head, tail)
            case None               => N.one
        }

      val range: Coalgebra[ListF[Int, ?], Int] =
        n => if (n > 0) Nested(Some((n, n - 1))) else Nested(None)
    }

    object nel {

      type NonEmptyListF[A, B] = Nested[(A, ?), Option, B]
      type NonEmptyList[A] = Fix[NonEmptyListF[A, ?]]

      def cons[A](head: A, tail: NonEmptyList[A]): NonEmptyList[A] =
        Fix(Nested((head, Some(tail))))

      def one[A](a: A): NonEmptyList[A] =
        Fix(Nested((a, none)))

      def product[A](implicit N: Numeric[A]): Algebra[NonEmptyListF[A, ?], A] =
        fa =>
          fa.value match {
            case (head, Some(tail)) => N.times(head, tail)
            case (head, None)       => head
        }

      val range: Coalgebra[NonEmptyListF[Int, ?], Int] =
        n => if (n > 1) Nested((n, (n - 1).some)) else Nested((1, none))
    }
  }
}

object ListExample extends App {

  import ListF._
  import ListF.algebras._

  val list: MyList[Int] =
    cons(1, cons(2, cons(3, cons(4, cons(5, nil)))))

  println(list.cata(sum))
  println(list.cata(product))
  println(list.cata(size))
  println(list.cata(map(_ * 10)))

  // Output:
  // 15
  // 120
  // 5
  // Fix(Cons(10,Fix(Cons(20,Fix(Cons(30,Fix(Cons(40,Fix(Cons(50,Fix(Nil)))))))))))

  val scalaList = List(1, 2, 3, 4, 5)

  println(scalaList.cata(sum))
  println(scalaList.cata(product))
  println(scalaList.cata(size))
  println(scalaList.cata(map(_ * 10)))

  // Output:
  // 15
  // 120
  // 5
  // Fix(Cons(10,Fix(Cons(20,Fix(Cons(30,Fix(Cons(40,Fix(Cons(50,Fix(Nil)))))))))))

  println(5.ana[MyList[Int]](coalgebras.range))
  println(5.ana[List[Int]](coalgebras.range))

  // Output:
  // Fix(Cons(5,Fix(Cons(4,Fix(Cons(3,Fix(Cons(2,Fix(Cons(1,Fix(Nil)))))))))))
  // List(5, 4, 3, 2, 1)

  def factorial(n: Int): Int =
    hylo(algebras.product[Int], coalgebras.range)(n)

  println(factorial(5)) // 120

  {
    import ListF.compose.list._

    val list: List[Int] =
      cons(1, cons(2, cons(3, cons(4, cons(5, nil)))))

    println(list)
    println(list.cata(product))

    val `3!` = hylo(product[Int], range)(3)
    println(`3!`)

    // Output:
    // List(5, 4, 3, 2, 1)
    // 120
    // Fix(Nested(Some((1,Fix(Nested(Some((2,Fix(Nested(Some((3,Fix(Nested(Some((4,Fix(Nested(Some((5,Fix(Nested(None))))))))))))))))))))))
    // 120
    // 6
  }

  {
    import ListF.compose.nel._

    val nel: NonEmptyList[Int] =
      cons(1, cons(2, cons(3, cons(4, one(5)))))

    println(nel)
    println(nel.cata(product))

    val `3!` = hylo(product[Int], range)(3)
    println(`3!`)

    // Output:
    // Fix(Nested((1,Some(Fix(Nested((2,Some(Fix(Nested((3,Some(Fix(Nested((4,Some(Fix(Nested((5,None)))))))))))))))))))
    // 120
    // 6
  }
}
