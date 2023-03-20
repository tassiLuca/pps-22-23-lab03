package u03

import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Lists.List

class LabTest:
  import List.*
  import Lab.*

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
