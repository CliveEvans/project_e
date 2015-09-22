import org.specs2.mutable.Specification

class ProblemTwoSpec extends Specification {

  def fib(x: Int): Int = {
    var minusOne = 1
    var minusTwo = 0
    var r = 0
    for (i <- 0 to x - 1) {
      r = minusOne + minusTwo
      minusTwo = minusOne
      minusOne = r
    }
    return r
  }


  "simple fib" should {
    "give 1 for 1" in {
      fib(1) should be_==(1)
    }

    "give 2 for 2" in {
      fib(2) should be_==(2)
    }

    "give 5 for 4" in {
      fib(4) should be_==(5)
    }
  }

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
