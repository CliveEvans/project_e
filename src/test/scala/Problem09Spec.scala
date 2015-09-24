import org.scalacheck.{Gen, Prop}
import org.specs2.ScalaCheck
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification

class Problem09Spec extends Specification with ScalaCheck {
  import Pythagorean._
  /*
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
   */

  def haveABCInAscendingOrder: Matcher[(Int, Int, Int)] = { t: (Int, Int, Int) =>
    (t._1 < t._2 && t._2 < t._3, " is not in ascending order")
  }

  def sumTo(value: Int): Matcher[(Int, Int, Int)] = { t: (Int, Int, Int) =>
    (t._1 + t._2 + t._3 == value, s" doesn't sum to $value")
  }

  "generated tuples" should {
    "sum to the correct value" in {
      Prop.forAll(Gen.chooseNum(6, 100)) { (sum: Int) =>
        val tuples: Seq[(Int, Int, Int)] = tuple3Generator(sum)

        tuples should contain(sumTo(sum))
      }

    }
    "obey a < b < c" in {
      val tuples: Seq[(Int, Int, Int)] = tuple3Generator(1000)
      tuples.toStream should contain(haveABCInAscendingOrder).forall
    }
  }

  "isPythagorean" should {
    "correctly identify (3,4,5) as Pythagorean" in {
      isPythagorean((3,4,5)) should beTrue
    }
    "realise (3,4,6) isn't" in {
      isPythagorean((3,4,6)) should beFalse
    }
  }
}

