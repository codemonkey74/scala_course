package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("parlineOfSight should invoke parallel construct 30 times for array size 17 and threshold 1") {
    val output = new Array[Float](17)
    val input = new Array[Float](17)
    0 until 17 foreach ( i => input(i) = i * 1f )
    parLineOfSight(input, output, 1)
    assert(output === 0f +: (1 until 17).map(_ => 1.0f))
  }

  test("upsweep should correctly handle the chunk 1 until 5 of an array of 5 elements") {
    val res = upsweep(Array[Float](0f, 7f, 10f, 33f, 48f), 1, 5, 1)
    assert(res === Node(Node(Leaf(1,2,7.0f),Leaf(2,3,5.0f)),Node(Leaf(3,4,11.0f),Leaf(4,5,12.0f))))
  }

  test("downsweep should compute correctly") {
    val output = new Array[Float](5)
    val res = downsweep(Array[Float](0f, 7f, 10f, 33f, 48f), output, 8.0f, Node(Node(Leaf(1,2,7.0f),Leaf(2,3,5.0f)),Node(Leaf(3,4,11.0f),Leaf(4,5,12.0f))))
    assert(output === List(0.0, 8.0, 8.0, 11.0, 12.0))
  }
}

