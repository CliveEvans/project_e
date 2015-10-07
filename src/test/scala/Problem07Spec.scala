import Primes._
import org.specs2.mutable.Specification

class Problem07Spec extends Specification {

  /*
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
   */

  "all primes" should {
    "give me 13 as the sixth" in {
      Stream.from(2).filter(i => isPrime(i.toLong)).drop(5).head should be_==(13)
    }

    "give me the 100001st" in {
      Stream.from(2).filter(i => isPrime(i.toLong)).drop(10000).head should be_==(104743)
    }
  }
}
