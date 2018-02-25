package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    from until end foreach { x =>
      0 until src.height foreach { y =>
        dst.update(x, y, boxBlurKernel(src, x, y, radius))
      }
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val colsPerTask = src.width / numTasks
    val extraCols = src.width % numTasks
    val fromsExtra = Range(0, extraCols * (colsPerTask + 1), colsPerTask + 1)
    val froms = fromsExtra ++ (if(colsPerTask > 0) Range(fromsExtra.end, src.width, colsPerTask) else Seq.empty)
    val fromTos: Seq[(Int, Int)] = froms.zip(froms.drop(1) :+ src.width)
    val tasks = fromTos.map {
      case (colFrom, colTo) => task(blur(src, dst, colFrom, colTo, radius))
    }
    tasks.foreach(_.join())
  }

}
