package funintset

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class EmptySet extends IntSet {

  def incl(x: Int): IntSet = new NonEmptyIntSet(x, new EmptySet, new EmptySet)

  def contains(x: Int): Boolean = false

  override def toString: String = "."

  def union(other: IntSet): IntSet = other
}

case class NonEmptyIntSet(i: Int, left: IntSet, right: IntSet) extends IntSet {

  def incl(x: Int): IntSet =
    if (x < i) new NonEmptyIntSet(i, left incl x, right)
    else if (x > i) new NonEmptyIntSet(i, left, right incl x)
    else this

  def contains(x: Int): Boolean =
    if (x < i) left contains x
    else if (x > i) right contains x
    else true

  override def toString: String = "{" + left + i + right + "}"

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl i
}
