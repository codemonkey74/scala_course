package list

import java.util.NoSuchElementException

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {

  override def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List {
  def findNth[T](n: Int, l: List[T]): T =
    if (l.isEmpty) throw new IndexOutOfBoundsException()
    else if (n == 0) l.head
    else findNth(n - 1, l.tail)
}
