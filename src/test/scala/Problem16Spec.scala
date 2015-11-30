import org.specs2.mutable.Specification

class Problem16Spec extends Specification {

  /*
    2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

    What is the sum of the digits of the number 2^1000?
   */

  def powersOfTwo(n: Int):BigInt = {
    Seq.fill(n)(BigInt(2)).product
  }

  def sumDigits(n: BigInt) = {
    n.toString.split("").map(_.toLong).sum
  }

  "2 ^ 15" should {
    val twoToFifteen = powersOfTwo(15)
    "be 32768" in {
      twoToFifteen must be_==(32768)
    }

    "sum it's digits to 26" in {
      sumDigits(twoToFifteen) must be_==(26)
    }
  }

  "2 ^ 1000" should {
    "be much bigger" in {
      val twoToOneThousand = powersOfTwo(1000)
      sumDigits(twoToOneThousand) must be_==(1366)
    }
  }

}
