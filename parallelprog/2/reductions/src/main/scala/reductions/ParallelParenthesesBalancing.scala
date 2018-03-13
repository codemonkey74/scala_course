package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    val res = chars.foldLeft((true, 0)) {
      case ((s, op), '(') => (s, op + 1)
      case ((s, op), ')') => (s && (op - 1 >= 0), op - 1)
      case ((s, op), _) => (s, op)
    }
    res._1 && res._2 == 0
  }


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    /** Returns needed opens to the left of block and needed closes to the right */
    def traverse(idx: Int, until: Int): (Int, Int) = {
       chars.slice(idx,until).foldLeft (0, 0) {
         case ((op, cl), '(') => (op, cl + 1)
         case ((op, cl), ')') =>
           if (cl > 0) (op, cl - 1)
           else (op + 1, cl)
         case ((op, cl), _) => (op, cl)
       }
    }

    /** calls [[merge()]] for parallel result */
    def mergePar(p: ((Int, Int), (Int, Int))): (Int, Int) =
      merge(p._1, p._2)

    /** merges needs from left and right */
    def merge(left: (Int, Int), right: (Int, Int)): (Int, Int) = {
      val adj = left._2 - right._1  //combines eft and right
      if (adj >= 0)   //if positive, needs more closes to the far right
        (left._1, right._2 + adj)
      else            //if negative, needs more (-adj) opens to the far left
        (left._1 - adj, right._2)
    }

    /** calculates op/cl needs in parallel */
    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold)
        traverse(from, until)
      else {
        val mid = (from + until) / 2
        mergePar(parallel(reduce(from, mid), reduce(mid, until)))
      }
    }

    /** it is balanced if there are no opens or closes needed */
    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!
}
