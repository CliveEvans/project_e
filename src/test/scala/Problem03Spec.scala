import org.specs2.mutable.Specification
import CommonHelpers._

class Problem03Spec extends Specification {

  import ProblemThree._

  "prime factors" should {
    "be 3 of 9" in {
      primeFactors(9).toList should containTheSameElementsAs(Seq(3, 3))
    }
  }

  "largest prime factor" should {
    "be 5 of 15" in {
      primeFactors(15).head should be_==(5)
    }

    "be 5 of 20" in {
      primeFactors(20).head should be_==(5)
    }

    "be 29 of 13195" in {
      primeFactors(13195).head should be_==(29)
    }

    "be 6857 of 600851475143" in {
      primeFactors(600851475143L).head should be_==(6857)
    }

    "of 997799" in {
      primeFactors(997799) should containTheSameElementsAs(Seq(11, 90709))
    }
  }

}


