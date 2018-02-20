import scala.Stream._


object A {
   def a(s: => Int, step: => Int): Stream[Int] = s #:: a(s + step, step)

  val naturals = a(0, 1)

  val natOdds= naturals.map(_ * 2)
  val integers = naturals.flatMap(p => Seq(p, -p)).drop(1)
  integers.take(10).toList

  def fib(a: Int, b: Int): Stream[Int] = a #:: fib(b, a + b)

  val fib01 = fib(0, 1)
  fib01.take(10).toList
  fib01.take(5).toList

  fib01.take(12).toList

  Seq(Seq[Int]).flatten

}
