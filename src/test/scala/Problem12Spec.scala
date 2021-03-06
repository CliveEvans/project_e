import CommonHelpers._
import org.specs2.mutable.Specification

class Problem12Spec extends Specification {

  /*
  The sequence of triangle numbers is generated by adding the natural numbers.
  So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.
  The first ten terms would be:
    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

  We can see that 28 is the first triangle number to have over five divisors.

  What is the value of the first triangle number to have over five hundred divisors?
  */

  val triangles = {
    def tail(iter: Int, curr: Int):Stream[Int] = {
      curr #:: tail(iter + 1, iter + 1 + curr)
    }
    tail(1, 1)
  }

  "triangle numbers" should {
    "be 28 at 7" in {
      triangles.take(7).last should be_==(28)
    }
  }

  "factors" should {
    "give 1 and 7 for 7" in {
      factors(7) should containAllOf(Seq(1,7))
    }

    "give 1,2,3,6 for 6" in {
      factors(6) should containAllOf(Seq(1,2,3,6))
    }

    "include non prime factors" in {
      factors(28) should containAllOf(Seq(1, 2, 4, 7, 14, 28))
    }

    "not be slow if I don't evaluate it" in {
      factors(76576500)
      true
    }
  }

  "7th triangle" should {
    "have 5 factors" in {
      factors(triangles(6)) should containAllOf(Seq(1, 2, 4, 7, 14, 28))
    }

    "be the first with 5 or more factors" in {
      triangles.dropWhile(factors(_).size < 5).head should beEqualTo(28)
    }

  }

  "with 500 divisors" should {
    "be really big" in {
      triangles.dropWhile(factors(_).size < 500).head should beEqualTo(76576500)
    }
  }
}
