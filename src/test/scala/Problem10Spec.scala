import Primes._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification
import org.specs2.specification.AroundTimeout

class Problem10Spec extends Specification with AroundTimeout {
  "primes below" should {
    "give all primes below 10" in {
      primesBelow(10) should containTheSameElementsAs(Seq(2, 3, 5, 7))
    }
    "give primes below 1000000 in a reasonable time" in { implicit ee: ExecutionEnv =>
      sumPrimes(1000000) should be_==(37550402023L)
    }
    "sum to the right value 10" in {
      sumPrimes(10) should be_==(17)
    }
    "sum to the right value for 25" in {
      sumPrimes(25) should be_==(100)
    }
    "give me a big number" in {
      sumPrimes(2000000) should be_==(142913828922L)
    }
  }
}