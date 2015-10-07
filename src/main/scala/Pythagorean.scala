object Pythagorean extends App {

  def tuple3Generator(sum: Int): Seq[(Int, Int, Int)] = {
    for {
      c <- 3 to (sum - 3)
      b <- 2 to (sum - c - 1) if b < c
      a = sum - c  -b if a < b
    } yield (a, b, c)
  }

  def sqr(x:Int) = x*x

  def isPythagorean(tuple: (Int, Int, Int)): Boolean = tuple match {
    case (a, b, c) => (sqr(a) + sqr(b)) == sqr(c)
  }

  def product(t: (Int, Int, Int)) = t._1 * t._2 * t._3

  println(tuple3Generator(1000).view.find(isPythagorean).map(product))

}
