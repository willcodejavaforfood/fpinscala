package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(l: List[Int]) = 
    foldRight(l, 0)((x,y) => x + y)
  
  def product2(l: List[Double]) = 
    foldRight(l, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`, see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A])(h: A): List[A] = l match {
    case _ => Cons(h, l)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => l match {
      case Cons(h, t) => drop(t, n -1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h))
        dropWhile(tail(l), f)
      else
        l
  }

  def init[A](l: List[A]): List[A] = {
    def loop(from: List[A], to: List[A]): List[A] = from match {
      case Cons(h, Nil) => to
      case Cons(h, t) => loop(t, append(to, List(h)))
    }
    l match { 
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => loop(t, List(h))
    }
  }

  def length[A](l: List[A]): Int = {
    def loop(list: List[A], length: Int): Int = list match {
      case Cons(h, Nil) => length + 1
      case Cons(h, t) => loop(t, length + 1)
    }
    l match {
      case Nil => 0
      case Cons(h, Nil) => 1
      case Cons(h, t) => loop(t, 1)
    }
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = sys.error("todo")

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}
