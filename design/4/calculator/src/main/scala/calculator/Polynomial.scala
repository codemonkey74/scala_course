package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
      val bs = b()
      bs * bs - 4 * a() * c()
   }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val deltas = delta()
    val bs = b()
    val cs = c()
    val as = a()
    deltas match {
      case d if as == 0 || d < 0 => Set.empty[Double]
      case d if d == 0 => Set(-bs / (2 * as))
      case _ => Set(
        (-bs + Math.sqrt(deltas))/(2 * as),
        (-bs - Math.sqrt(deltas))/(2 * as)
      )
    }
  }
}
