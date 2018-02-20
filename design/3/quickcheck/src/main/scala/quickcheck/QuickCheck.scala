package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      h <- oneOf(
        const(empty),
        genHeap
      )
    } yield insert(x, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  @inline def isMin(a: Int, b: Int): Boolean = Math.min(a, b) == a

  def isMinHeap(h: H): Boolean = {
    def isMinTail(head: Int, tail: H): Boolean =
      isEmpty(tail) ||
        (isMin(head, findMin(tail)) && isMinTail(findMin(tail), deleteMin(tail)))

    isEmpty(h) || isMinTail(findMin(h), deleteMin(h))
  }

  def contains(ha: H, hb: H): Boolean = {
    if (isEmpty(hb)) true
    else {
      val a = findMin(ha)
      val b = findMin(hb)
      (a == b, a < b) match {
        case (true, _) => contains(ha, deleteMin(hb))
        case (_, true) => contains(deleteMin(ha), hb)
        case (false, false) => false
      }
    }
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOf2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    findMin(insert(b, h)) == min(a, b)
  }

  property("removeSingle") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("isSorted") = forAll { (h: H) =>
    isMinHeap(h)
  }

  property("meldingHasRightHead") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    (isEmpty(h1), isEmpty(h2)) match {
      case (true, true) => true
      case (true, false) => findMin(m) == findMin(h2)
      case (false, true) => findMin(m) == findMin(h1)
      case (false, false) => findMin(m) == Math.min(findMin(h1), findMin(h2))
    }
  }

  property("deleteIsSorted") = forAll { (h: H) =>
    isEmpty(h) || isMinHeap(deleteMin(h))
  }

  property("meldIsSorted") = forAll { (h1: H, h2: H) =>
    isMinHeap(meld(h1, h2))
  }

  property("meldHasAllElementsOfSecond") = forAll { (h1: H, h2: H) =>
    contains(meld(h1, h2), h2)
  }

  property("meldHasAllElementsOfFirst") = forAll { (h1: H, h2: H) =>
    contains(meld(h1, h2), h1)
  }

}
