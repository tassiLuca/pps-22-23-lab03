package u03

import org.junit.Assert.assertEquals
import org.junit.Test
import u02.Optionals.Option
import u02.Modules.{Person, isStudent}
import u03.Lists.List
import u03.Streams.*
import Lab.*
import List.*

class ListTests:

  val list: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test
  def testDrop(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), drop(list, 1))
    assertEquals(Cons(30, Nil()), drop(list, 2))
    assertEquals(Nil(), drop(list, 5))
    assertEquals(Nil(), drop(list, -1))

  @Test
  def testAppend(): Unit =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(list, Cons(40, Nil())))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), append(list, Nil()))
    assertEquals(Cons(40, Nil()), append(Nil(), Cons(40, Nil())))

  @Test
  def testFlatMap(): Unit =
    assertEquals(Nil(), flatMap(Nil[Int]())(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(list)(v => Cons(v + 1, Nil())))
    assertEquals(
      Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))),
      flatMap(list)(v => Cons(v + 1, Cons(v + 2, Nil())))
    )

  @Test
  def testMap(): Unit =
    assertEquals(Nil(), maps(Nil[Int]())(v => v * 3))
    assertEquals(Cons(1, Cons(2, Cons(3, Nil()))), maps(list)(v => v / 10))

  @Test
  def testFilter(): Unit =
    assertEquals(list, filters(list)(_ => true))
    assertEquals(Nil(), filters(list)(v => v > 30))
    assertEquals(Cons(10, Cons(20, Nil())), filters(list)(v => v < 30))

  import Option.*

  @Test
  def testMax(): Unit =
    assertEquals(Some(30), max(list))
    assertEquals(None(), max(Nil()))

  @Test
  def testFoldLeft(): Unit =
    assertEquals(10, foldLeft(Nil[Int]())(10)(_ - _))
    assertEquals(-60, foldLeft(list)(0)(_ - _))
    assertEquals(1, foldLeft(list)(6_000)(_ / _))
    assertEquals("a102030", foldLeft(list)("a")((i, v) => i.concat(s"$v")))

  @Test
  def testFoldRight(): Unit =
    assertEquals(10, foldRight(Nil[Int]())(10)(_ - _))
    assertEquals(20, foldRight(list)(0)(_ - _))
    assertEquals(5, foldRight(list)(3)(_ / _))
    assertEquals("102030a", foldRight(list)("a")((v, i) => s"$v".concat(i)))

class PersonTests:
  import Person.*

  @Test
  def testCoursesOf(): Unit =
    val p = Cons(Teacher("Pianini", "oop"), Cons(Student("Tassinari", 2000), Cons(Teacher("Ricci", "pcd"), Nil())))
    assertEquals(Cons("oop", Cons("pcd", Nil())), coursesOf(p))
    assertEquals(Nil(), coursesOf(Cons(Student("Pippo", 1990), Nil())))
    assertEquals(Nil(), coursesOf(Nil()))

class StreamTests:
  import Stream.*

  @Test
  def testDropStream(): Unit =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(s)(6)))
    assertEquals(Nil(), Stream.toList(Stream.drop(s)(10)))
    assertEquals(Nil(), Stream.toList(Stream.drop(Stream.empty())(6)))

  @Test
  def testConstantStream(): Unit =
    assertEquals(Cons("x", Cons("x", Cons("x", Nil()))), Stream.toList(Stream.take(Stream.constant("x"))(3)))

  @Test
  def testFibonacciSeries(): Unit =
    assertEquals(
      Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))),
      Stream.toList(Stream.take(fibs)(8))
    )
