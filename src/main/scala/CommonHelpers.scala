object CommonHelpers {

  def from(n:Long, stop: Long) : Stream[Long] = n match {
    case p if p >= stop => Stream.empty
    case _ => n #:: from(n + 1, stop)
  }

  def isFactor(i: Long)(x: Long): Boolean = {
    i % x == 0
  }

  def isPrime(i: Long): Boolean = {
    val sqrRtI = Math.sqrt(i).toInt
    ! from(2L, sqrRtI + 1).exists(isFactor(i))
  }

  def countDown(n: Long):Stream[Long] = {
    if (n == 1) {
      Stream.empty
    } else {
      Stream.cons(n, countDown(n-1))
    }
  }

}
