import org.specs2.mutable.Specification

import scala.collection.immutable.IndexedSeq

class Problem26Spec extends Specification {

  /*
    A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

    1/2	= 	0.5
    1/3	= 	0.(3)
    1/4	= 	0.25
    1/5	= 	0.2
    1/6	= 	0.1(6)
    1/7	= 	0.(142857)
    1/8	= 	0.125
    1/9	= 	0.(1)
    1/10	= 	0.1
    Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

    Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
   */


  def longDivision(numerator:Int, denominator:Int): Stream[Int] = {
    val result = numerator / denominator
    val remainder = numerator % denominator
    if (remainder == 0) {
      result #:: Stream.empty
    } else {
      result #:: longDivision(10 * remainder, denominator)
    }
  }

  def cycleLength(denominator: Int, maxDepth: Int = 1500): Option[Int] = {

    val noLeadingZeros: Stream[Int] = longDivision(1, denominator).dropWhile(_ == 0)

    if(noLeadingZeros.take(500).length < 500) {
      None
    } else {

      val chunks = Stream.from(1).takeWhile(_ < maxDepth).map { i: Int =>
        (noLeadingZeros.take(i).toList, noLeadingZeros.drop(i).take(i).toList)
      }

      def areEqual: ((List[Int], List[Int])) => Boolean = { case (left, right) => left == right }

      Stream.from(0)
        .takeWhile(_ < maxDepth)
        .view
        .flatMap(n => chunks.filter(t => t._1.length > n).map(t => (t._1.drop(n), t._2.drop(n))))
        .filter(areEqual).map(_._1.length).headOption
    }

  }

  "cycleLength" should {
    "give none for non-recurring numbers" in {
      cycleLength(2) must beNone
      cycleLength(4) must beNone
      cycleLength(5) must beNone
      cycleLength(10) must beNone
      cycleLength(1000000000) must beNone
    }

    "be some if they recur" in {
      cycleLength(3) must beSome(1)
      cycleLength(6) must beSome(1)
      cycleLength(7) must beSome(6)
      cycleLength(11) must beSome(2)
      cycleLength(29) must beSome(28)
    }

    "allow overriding max depth" in {
      cycleLength(983, maxDepth = 10000) must beSome(982)
    }
  }

  "long division" should {
    "be simple for non-recurring" in {
      longDivision(2,1) must containTheSameElementsAs(Seq(2))
      longDivision(1,2) must containTheSameElementsAs(Seq(0, 5))
      longDivision(1,4) must containTheSameElementsAs(Seq(0, 2, 5))
      longDivision(1,40) must containTheSameElementsAs(Seq(0, 0, 2, 5))
    }

    "keep going as deep as we want to look" in {
      longDivision(1,3).take(4) must containTheSameElementsAs(Seq(0, 3, 3, 3))
      longDivision(1,7).take(10) must containTheSameElementsAs(Seq(0, 1, 4, 2, 8, 5, 7, 1, 4, 2))
    }
  }

  "from 1 to 1000 there" should {
    "be a longest recurring chain" in {
      val cycleLengths= (1 to 1000).map((denominator: Int) => cycleLength(denominator)).zipWithIndex
      val head = cycleLengths.sortBy(_._1.getOrElse(0)).reverse.head
      head._1 must beSome(982)
      (head._2 + 1) must be_==(983)
    }
  }
}
