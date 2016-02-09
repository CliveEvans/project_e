object CommonHelpers {

  def from(n:Long, stop: Long) : Stream[Long] = n match {
    case p if p >= stop => Stream.empty
    case _ => n #:: from(n + 1, stop)
  }

  def countDown(n: Long):Stream[Long] = {
    if (n == 1) {
      Stream.empty
    } else {
      Stream.cons(n, countDown(n-1))
    }
  }

  def factors(i: Long): Stream[Int] = {
    def withFactor(f: Int):Seq[Int] = Seq(f, (i/f).toInt)
    Stream.from(1)
      .takeWhile(_ <= Math.sqrt(i))
      .filter(isFactor(i)(_))
      .flatMap(withFactor)
  }

  def isFactor(i: Long)(x: Long): Boolean = {
    i % x == 0
  }

}
