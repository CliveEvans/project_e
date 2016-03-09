import org.specs2.mutable.Specification

class Problem27Spec extends Specification {

  /**
    * Euler discovered the remarkable quadratic formula:
    * n² + n + 41

    * It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39.

    * However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly
    * when n = 41, 41² + 41 + 41 is clearly divisible by 41.

    * The incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive values
    * n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.

    * Considering quadratics of the form:
    * n² + an + b, where |a| < 1000 and |b| < 1000
    * where |n| is the modulus/absolute value of n
    * e.g. |11| = 11 and |−4| = 4

    * Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number
    * of primes for consecutive values of n, starting with n = 0.
    */
  "the number of consecutive primes" should {

    "be 40 when a is 1 and b 41" in {
      numberOfPrimes(1, 41) must be_==(40)
    }

    "be 80 when a is 79 and b is 1601" in {
      numberOfPrimes(-79, 1601) must be_==(80)
    }
  }


  "primesInRange" should {
    "work for defined instance" in {
      numberOfPrimesInRange(1 to 1, 41 to 41) must be_==(Seq((40, (1, 41))))
    }

    "find the biggest" in {
      sortedPrimesInRange(0 to 5, -41 to 41).head must be_==((40, (1, 41)))
    }
  }

  def largestCoefficient(aIn: Range, bIn: Range):Long = {
    val (numberOfPrimes, (a, b)) = sortedPrimesInRange(aIn, bIn).head
    val coefficient = a * b
    println(s"Found $numberOfPrimes at ($a, $b): $coefficient")
    coefficient
  }

  "the coefficient of the largest" should {

    "match our examples" in {
      largestCoefficient(0 to 5, -41 to 41) must be_==(41)
      largestCoefficient(-80 to -70, 1580 to 1605) must be_==(-126479)
    }

    "should be correct when in the required range" in {
      largestCoefficient(-999 to 999, -999 to 999) must be_==(-59231)
    }
  }

  def sortedPrimesInRange(aIn: Range, bIn: Range):Seq[(Int, (Int, Int))] = numberOfPrimesInRange(aIn, bIn).sortBy(-_._1)

  def numberOfPrimesInRange(aIn: Range, bIn: Range):Seq[(Int, (Int, Int))] = {
    for {
      a <- aIn if a % 2 != 0
      b <- bIn if Primes.isPrime(b)
    } yield (numberOfPrimes(a, b),(a,b))
  }

  def numberOfPrimes(a: Long, b: Long):Int = {
    Stream.from(0).view.map(n => n * n + a * n + b).takeWhile(Primes.isPrime).length
  }

}
