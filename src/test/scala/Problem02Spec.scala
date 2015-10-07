import org.specs2.mutable.Specification

class Problem02Spec extends Specification {

  def sumEvenFib(lessThan: Int): BigInt = {
    def sumIfEven(first: Int, second:Int, acc:BigInt):BigInt = {
      val fib = first + second
      fib match {
        case f if f >= lessThan => acc
        case f if f % 2 == 0 => sumIfEven(second, fib, fib + acc)
        case _ => sumIfEven(second, fib, acc)
      }
    }

    sumIfEven(0, 1, 0)
  }

  "sum of even fibbonacci" should {
    "be 2 below 5" in {
      sumEvenFib(5) should be_==(2)
    }

    "be 10 below 9" in {
      sumEvenFib(9) should be_==(10)
    }

    "be really big below 4 million" in {
      val huge = sumEvenFib(4000000)

      huge should be_==(4613732)
    }
  }
}
