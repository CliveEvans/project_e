import org.specs2.mutable.Specification

class Problem20Spec extends Specification {

  /*
    n! means n × (n − 1) × ... × 3 × 2 × 1

    For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
    and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

    Find the sum of the digits in the number 100!
   */

  "factorial" should {
    "be correct" in {
      f(1) must be_==(1)
      f(2) must be_==(2)
      f(3) must be_==(6)
      f(10) must be_==(3628800)
    }

    "not integer ovberflow" in {
      f(100) must (be_<(f(101)) and be_>(f(99)) and be_>(BigInt(Integer.MAX_VALUE)))
    }
  }

  "sum of values" should {
    "add all values in a BigInt" in {
      sumDigits(BigInt(13042)) must be_==(10)
    }

    "give me a sensible answer" in {
      sumDigits(f(100)) must be_==(648)
    }
  }


  def f(n: BigInt):BigInt = {
    (BigInt(1) to n).product
  }

  def sumDigits(n: BigInt): BigInt = {
    n.toString().split("").map(BigInt.apply).sum
  }

}
