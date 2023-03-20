package u03

import u03.Lists.*

import scala.annotation.tailrec

object Lab extends App:
  import List.*

  @tailrec
  def drop[A](list: List[A], index: Int): List[A] = (list, index) match
    case (l, 0) => l
    case (Nil(), _) => Nil()
    case (Cons(_, t), i) => drop(t, i - 1)

  def append[A](left: List[A], right: List[A]): List[A] = left match
    case Nil() => right
    case Cons(h, Nil()) => Cons(h, right)
    case Cons(h, t) => Cons(h, append(t, right))

