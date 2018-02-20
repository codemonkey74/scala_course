package funsets

import scala.annotation.tailrec

import scala.languageFeature.implicitConversions


object F {

  trait AA {
    def a = 1
  }

  class AA1 extends AA {}

  class AA2 extends AA {

    override def a: Int = 2
  }

  class BB1 {}

  def foo[T <: AA](t: T): Int = t.a

  foo(new AA1)
  foo(new AA2)



  val       a = 1    //evaluated once and assigned
  lazy val aa = { 1 / 0 }    //evaluated once on the first call and assigned
  def      aaa = 1   //evaluated on every call


  trait DB {
    abstract def createConnection: String = ???

  }

  trait E1 extends DB {
    abstract override def createConnection: String = super.createConnection
    lazy val db: String = createConnection

    db.charAt(1)
  }

  trait MongoE1[T] extends DB { myself: Numeric[T] =>
    abstract override def createConnection: String = super.createConnection
  }

  class Mongo[T] extends E1 with MongoE1[T] with Numeric[T] {

  }

  lazy val AO = new {
      val b = 1
    }


  AO.b
}







/**
 * 2. Purely Functional Sets.
 */
trait FunSets[T : Numeric] {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = T => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: T): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
    def singletonSet(elem: T): Set = x => x == elem


  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
    def union(s: Set, t: Set): Set = x => s(x) || t(x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = x => s(x) && t(x)


  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = x => s(x) && ! t(x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: T => Boolean): Set = x => s(x) && p(x)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound: T

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {

    @tailrec
    def iter(a: T): Boolean = {
      if (a > bound) true
      else if (s(a) && ! p(a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: T => Boolean): Boolean = !forall(s, !p(_))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: T => Int): Set = x => exists(s, y => f(y) == x)


  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}


object FunSetInt extends FunSets[Int]  {
  val frombound: Int = 1000
}
