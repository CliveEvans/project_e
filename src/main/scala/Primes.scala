object Primes {

  import CommonHelpers._

  def isPrime(i: Long): Boolean = {
    if (i < 1) false
    else if (i == 2) true
    else if ((i & 1) == 0) false
    else {
      val sqrt: Double = Math.sqrt(i)
      primes.view.takeWhile(_ <= sqrt).forall(!isFactor(i)(_))
    }
  }

  def primeFactors(i: Long): Stream[Long] = i match {
    case 1 => Stream.empty
    case p if isPrime(p) => Stream.cons(p, Stream.empty)
    case _ =>
      firstPrimeFactor(i).map(p => primeFactors(i/p) :+ p).getOrElse(Stream.empty)
  }

  def firstPrimeFactor(i: Long): Option[Long] = {
    val root = Math.sqrt(i)
    primes.view.takeWhile(_ <= root).find(isFactor(i))
  }

  def primesBelow(limit: Int): Seq[Long] = {
    primes.takeWhile(_ <= limit)
  }

  def sieve(s: Stream[Long]): Stream[Long] = s match {
    case head #:: tail => head #:: sieve(tail.filter(_ % head != 0))
    case _ => Stream.empty
  }

  val primes: Stream[Long] = {
    def nextPrime(i: Long): Long = {
      if (isPrime(i)) i else nextPrime(i + 2)
    }
    def tail(i: Long): Stream[Long] = {
      i #:: tail(nextPrime(i + 2))
    }

    2 #:: tail(3)
  }

  def sumPrimes(limit: Int): Long = {
    val root = Math.sqrt(limit)
    def next(i: Int, acc: Long, knownPrimes: Seq[Int]): Long = i match {
      case n if n > limit => acc
      case p if isPrime(p, knownPrimes) => {
        if (p <= root)
          next(p + 2, acc + p, knownPrimes :+ p) // if we prepend we'd have to reverse when we iterate
        else
          next(p + 2, acc + p, knownPrimes)
      }
      case _ => next(i + 2, acc, knownPrimes) // no point in checking even numbers
    }
    def isPrime(i: Int, knownPrimes: Seq[Int]): Boolean = {
      val rootOfI: Double = Math.sqrt(i)
      knownPrimes.view.takeWhile(_ <= rootOfI).forall(i % _ != 0)
    }
    next(3, 2, Vector(2)) // effectively constant time for append on Vector
  }

}
