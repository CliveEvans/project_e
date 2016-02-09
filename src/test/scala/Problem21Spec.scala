import CommonHelpers._

import org.specs2.mutable.Specification

import scala.collection.immutable.IndexedSeq

class Problem21Spec extends Specification {

  /*
    Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
    If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

    For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
    The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

    Evaluate the sum of all the amicable numbers under 10000.
  */


  "d(220)" should {
    "be 284" in {
      d(220) must be_==(284)
    }
  }

  "d(284)" should {
    "be 220" in {
      d(284) must be_==(220)
    }
  }


  "amicable numbers" should {
    "contain (220, 284)" in {
      val expected = Seq(220, 284)
      amicableNumbersBelow(300) must havePair(220 -> 284)
      amicableNumbersBelow(300) must havePair(284 -> 220)
    }
  }

  "all amicable below 10000" should {
    "sum to a big number" in {
      val amicableNumbers: Seq[(Int, Int)] = amicableNumbersBelow(10000)
      val sumOfAmicable: Int = amicableNumbers.map { case (a, b) => a + b }.sum / 2
      sumOfAmicable must be_==(31626)
    }
  }


  def d(n:Int):Int = {
    factors(n).filter(_ != n).sum
  }

  def amicableNumbersBelow(n: Int): Seq[(Int,Int)] = {

    def amicableOrNothing(i:Int):Option[(Int,Int)] = {
      val possibleAmicable: Int = d(i)
      if (d(possibleAmicable) == i && possibleAmicable != i) {
        Some(i, possibleAmicable)
      } else {
        None
      }
    }

    (1 to n).flatMap(amicableOrNothing)
  }

}
