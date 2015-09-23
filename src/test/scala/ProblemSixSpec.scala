import org.specs2.mutable.Specification

class ProblemSixSpec extends Specification {

  /*
The sum of the squares of the first ten natural numbers is,

1^2 + 2^2 + ... + 10^2 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)^2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
   */

  "naively" should {
    "give me the sum of the first 100 squares" in {
      val sumOfSquares = (1 to 100).map(sqr).fold(0)(sum)
      val sums = (1 to 100).fold(0)(sum)
      (sums * sums) - sumOfSquares should be_==(2640)
    }
  }

  def sqr(i: Int):Int = i * i

  def sum(i: Int, j: Int) = i + j

}
