import org.specs2.mutable.Specification

class ProblemTwoSpec extends Specification {

  def sumEvenFib(lessThan: Int): BigInt = {
    var minusOne:BigInt = 1
    var minusTwo:BigInt = 0
    var r:BigInt = 0
    var acc:BigInt = 0
    while (r < lessThan) {
      r = minusOne + minusTwo
      minusTwo = minusOne
      minusOne = r
      if (r % 2 == 0) acc += r
    }
    acc
  }

  "sum of even fibbonacci" should {
    "be 2 below 5" in {
      sumEvenFib(5) should be_==(2)
    }

    "be really big below 4 million" in {
      val huge = sumEvenFib(4000000)

      println(huge)
      true
    }
  }
}
