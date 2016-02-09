import CommonHelpers.divisors
import org.scalacheck.{Gen, Prop}
import org.specs2.ScalaCheck
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification

import scala.collection.immutable.{IndexedSeq, TreeSet}

class Problem23Spec extends Specification with ScalaCheck {


  /*
    A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example,
    the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

    A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this
    sum exceeds n.

    As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum
    of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can
    be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis
    even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is
    less than this limit.

    Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
   */


  def isAbundant(i: Int): Boolean = {
    divisors(i).sum > i
  }

  def isSumOfAbundant(i: Int): Boolean = {
    val candidates = abundantNumbersBelow28124.takeWhile(_ <= i)
    candidates.exists(n => candidates.contains(i - n))
  }

  val abundantNumbersBelow28124 = TreeSet.empty[Int] ++ (12 to 28123).filter(isAbundant)

  "isAbundant" should {
    "return false for all numbers up to 12" in {
      (1 to 11) must contain(not(beAbundant)).forall
    }

    "return true for 12" in {
      isAbundant(12) must beTrue
    }

    "return true for 20" in {
      20 must beAbundant
    }
  }

  "abundantNumbers" should {
    "start from 12" in {
      abundantNumbersBelow28124.head must be_==(12)
    }
  }

  "isSumOfAbundant" should {
    "not be true below 24" in {
      (1 to 23) must contain(not(beSumOfAbundant))
    }
    "give true for 24" in {
      isSumOfAbundant(24) must beTrue
    }

    "give true for 12 + 20" in {
      32 must beSumOfAbundant
    }

    "give true for any pair of abundant numbers" in {
      def atPosition(i: Int): Int = {
        abundantNumbersBelow28124.drop(i).head
      }

      val inRange = Gen.choose(0, abundantNumbersBelow28124.size)
      Prop.forAll(inRange, inRange){(first:Int, second:Int) =>
        val sum = atPosition(first) + atPosition(second)
        sum must beSumOfAbundant
      }
    }

  }

  "all number not the sum of two abundant numbers" should {
    "add up to quite a bit" in {
      (1 to 28124).filterNot(isSumOfAbundant).map(BigInt.apply).sum must be_==(4179871)
    }
  }

  def beSumOfAbundant: Matcher[Int] = {
    (i: Int) => (isSumOfAbundant(i), s"$i is not the sum of a pair of abundant numbers")
  }

  def beAbundant: Matcher[Int] = {
    i:Int => (isAbundant(i), s"$i is not abundant")
  }
}
