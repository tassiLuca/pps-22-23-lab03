package u03

import u03.Lists.*
import u02.Optionals.*
import u02.Modules.*
import u03.Streams.*

import scala.annotation.tailrec

object Lab extends App:
  import List.*
  import Option.*

  // Task 1.1.a
  @tailrec
  def drop[A](list: List[A], index: Int): List[A] = (list, index) match
    case (Nil(), _) => Nil()
    case (l, 0) => l
    case (Cons(_, t), i) => drop(t, i - 1)

  // Task 1.1.b
  def append[A](left: List[A], right: List[A]): List[A] = left match
    case Nil() => right
    case Cons(h, Nil()) => Cons(h, right)
    case Cons(h, t) => Cons(h, append(t, right))

  // Task 1.1.c
  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list match
    case Nil() => Nil()
    case Cons(h, Nil()) => f(h)
    case Cons(h, t) => append(f(h), flatMap(t)(f))

  // Task 1.1.d
  def maps[A, B](list: List[A])(f: A => B): List[B] = flatMap(list)(a => Cons(f(a), Nil()))

  def filters[A](list: List[A])(predicate: A => Boolean): List[A] =
    flatMap(list)(a => predicate(a) match
      case true => Cons(a, Nil())
      case _ => Nil()
    )

  // Task 1.2
  def max(list: List[Int]): Option[Int] = list match
    case Nil() => None()
    case Cons(h, Nil()) => Some(h)
    case Cons(h, t) => max(t) match
      case Some(x) if x > h => Some(x)
      case _ => Some(h)

  // Task 2.2
  @tailrec
  def foldLeft[A, B](list: List[A])(initial: B)(aggregator: (B, A) => B): B = list match
    case Nil() => initial
    case Cons(h, t) => foldLeft(t)(aggregator(initial, h))(aggregator)

  def foldRight[A, B](list: List[A])(initial: B)(aggregator: (A, B) => B): B =
    def reverse[E](list: List[E]): List[E] = list match
      case Cons(_, Nil()) => list
      case Cons(h, t) => append(reverse(t), Cons(h, Nil()))
      case _ => Nil()
    foldLeft(reverse(list))(initial)((b, a) => aggregator(a, b))

  // Task 2.1
  import Person.*

//  def coursesOf(list: List[Person]): List[String] =
//    maps(filters(list)(p => !isStudent(p)))(p => p match { case Teacher(_, c) => c} )

  def coursesOf(list: List[Person]): List[String] =
    flatMap(list)(_ match
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()
    )

  // Task 3.3
  val fibs: Stream[Int] =
    Stream.map(Stream.iterate((0, 1))((p0, p1) => (p1, p1 + p0)))((v, _) => v)


