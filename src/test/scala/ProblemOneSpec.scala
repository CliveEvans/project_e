import org.specs2.mutable.Specification

class ProblemOneSpec extends Specification {

  def multiplesBelow(number: Int):Seq[Int] = (1 to number - 1).filter(divisible)

  def divisible(x: Int): Boolean = {
    x % 3 == 0 || x% 5 == 0
  }

  "multiples of 3 and 5" should {
    "give nothing below 3" in {
      multiplesBelow(3) should beEmpty
    }

    "give 3 below 4" in {
      multiplesBelow(4) should contain(3)
    }

    "give 5 below 6" in {
      multiplesBelow(6) should contain(5)
    }

    "natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9" in {
      multiplesBelow(10) should contain(3,5,6,9)
    }
  }

  def sumMultipleBelow(i: Int):Int = {
    val below: Seq[Int] = multiplesBelow(i)
//    println(below)
    below.fold(0)(_ + _)
  }

  "sum multiples below 10" should {
    "be 23" in {
      sumMultipleBelow(10) should be_==(23)
    }
  }

  "my answer should be" in {
    val answer: Int = sumMultipleBelow(1000)
    println(answer)
    answer should be_>(23)
  }

}
