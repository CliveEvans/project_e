import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange.Inclusive
import scala.collection.mutable

class Problem14Spec extends Specification with ScalaCheck {

  sequential
  /*
    The following iterative sequence is defined for the set of positive integers:

    n → n/2 (n is even)
    n → 3n + 1 (n is odd)

    Using the rule above and starting with 13, we generate the following sequence:

    13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
    It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

    Which starting number, under one million, produces the longest chain?

    NOTE: Once the chain starts the terms are allowed to go above one million.
   */

  object Chain {

    def next(n: Long):Long = n match {
      case i if (i <= 0) => throw new IllegalArgumentException(s"$i is out of range")
      case 1 => 1
      case i if i%2 == 0 => i/2
      case _ => n*3 + 1
    }

    def chain(n: Long): Seq[Long] = {
      def buildChain(i: Long, acc: Seq[Long]): Seq[Long] = i match {
        case 1 => acc :+ 1L
        case _ => if (chains.contains(i)) {
          acc ++ chains(i)
        } else {
          buildChain(next(i), acc :+ i)
        }
      }
      chains.getOrElseUpdate(n, buildChain(n, Vector.empty))
    }

    val chains:mutable.Map[Long, Seq[Long]] = {
      mutable.Map()
    }



    def lengthOfChain(i: Long): Long = {
      def chainSize(i: Long, acc: Long): Long = i match {
        case 1 => acc + 1
        case _ => if (chainSizes.contains(i)) {
          acc + chainSizes(i)
        } else {
          chainSize(next(i), acc + 1)
        }
      }
      chainSizes.getOrElseUpdate(i, chainSize(i, 0))
    }

    val chainSizes:mutable.Map[Long, Long] = {
      mutable.Map()
    }

    (1 to 40)
      .map(Math.pow(2, _).toLong)
      .takeWhile(_ < Long.MaxValue)
      .map(lengthOfChain)


    def longest(in: Inclusive[Long]): Seq[Long] = {
      @tailrec
      def bestRemaining(remaining: Seq[Long], best: Seq[Long]):Seq[Long] = remaining match {
        case head::tail => {
          val thisChain = chain(head)
          if (thisChain.size > best.size) {
            bestRemaining(tail, thisChain)
          } else {
            bestRemaining(tail, best)
          }
        }
        case Nil => best
      }
      bestRemaining(in.toList, Nil)
    }
  }

  import Chain._
  val positiveIntegers = Gen.choose(1, Long.MaxValue)
  "next chain element" should {
    "be 1 if n is 1" in {
      next(1) must be_==(1)
    }
    "be 40 if n is 13" in {
      next(13) must be_==(40)
    }
    "be n/2 if n is even" in {
      prop((i:Long) => (i%2 == 0) ==>
        (next(i) must be_==(i/2))
      )
    }.setGen(positiveIntegers)
    "be n*3 + 1 if n is odd" in {
      prop((i:Long) => (i%2 == 1 && i > 2) ==>
        (next(i) must be_==(i*3 + 1))
      ).setGen(positiveIntegers)
    }
  }

  "a chain" should {
    "be 1 for 1" in {
      chain(1) must be_==(Seq(1))
    }
    "be correct, starting with 8" in {
      chain(8) must be_==(Seq(8,4,2,1))
    }
    "be correct starting with 13" in {
      chain(13) must haveSize(10)
    }
  }

  "chain sizes" should {
    "be 10 for with 13" in {
      lengthOfChain(13) must be_==(10)
    }
    "work when caching" in {
      lengthOfChain(26) must be_==(11)
      lengthOfChain(52) must be_==(12)
    }
  }

  "the chain for a million" should {
    "run in sensible time" in {
      chain(1000000) must haveSize(153)
    }
    "cache the answer" in {
      chain(1000000) must haveSize(153)
    }
  }

  "the longest chain" >> {
    "less than or equal to 8" should {
      "start at 7" in {
        longest(1L to 8L).head must be_==(7)
      }
    }

    "less than or equal to 500" should {
      "make sense" in {
        longest(1L to 500L).head must be_==(327)
      }
    }

    "less than 1 million" should {
      "be really big" in {
        (1L to 999999L).map(i => (i, lengthOfChain(i))).sortBy(t => -t._2).head._1 must be_==(837799L)
      }
    }

    "the other way" should {
      "give the same answer" in {
        longest(1L to 999999L).head must be_==(837799L)
      }
    }
  }

}
