import org.specs2.mutable.Specification

class Problem01Spec extends Specification {

  def naturalNumbersBelow(number: Int):Seq[Int] = (1 to number - 1).filter(divisible)

  def divisible(x: Int): Boolean = {
    x % 3 == 0 || x% 5 == 0
  }

  "multiples of 3 and 5" should {
    "give nothing below 3" in {
      naturalNumbersBelow(3) should beEmpty
    }

    "give 3 below 4" in {
      naturalNumbersBelow(4) should contain(3)
    }

    "give 5 below 6" in {
      naturalNumbersBelow(6) should contain(5)
    }

    "natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9" in {
      naturalNumbersBelow(10) should contain(3,5,6,9)
    }
  }

  def sumMultipleBelow(i: Int):Int = naturalNumbersBelow(i).sum

  "sum multiples below 10" should {
    "be 23" in {
      sumMultipleBelow(10) should be_==(23)
    }
  }

  "my answer should be" in {
    sumMultipleBelow(1000) should be_==(233168)
  }

}
