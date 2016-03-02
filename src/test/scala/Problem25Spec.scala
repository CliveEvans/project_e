import org.scalacheck.{Gen, Prop}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class Problem25Spec extends Specification with ScalaCheck {

  /*
    The Fibonacci sequence is defined by the recurrence relation:
  
    Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
    Hence the first 12 terms will be:
    
    F1 = 1
    F2 = 1
    F3 = 2
    F4 = 3
    F5 = 5
    F6 = 8
    F7 = 13
    F8 = 21
    F9 = 34
    F10 = 55
    F11 = 89
    F12 = 144
    The 12th term, F12, is the first term to contain three digits.
    
    What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
   */

  def fib(i: Int):BigInt = i match {
    case n if n <= 2 => 1
    case _ => fib(i-1) + fib(i-2)
  }

  def f(i: Int):BigInt = i match {
    case n if n < 1 => throw new IndexOutOfBoundsException
    case _ => fibs(i-1)
  }

  val fibs: Stream[BigInt] = {
    def rest(prevOne: BigInt, prevTwo:BigInt):Stream[BigInt] = {
      (prevOne + prevTwo) #:: rest(prevTwo, prevOne + prevTwo)
    }
    1 #:: rest(0, 1)
  }

  "fib" should {
    "be correct" in {
      fib(1) must be_==(1)
      fib(2) must be_==(1)
      fib(3) must be_==(2)
      fib(4) must be_==(3)
      fib(5) must be_==(5)
      fib(6) must be_==(8)
      fib(7) must be_==(13)
      fib(8) must be_==(21)
      fib(9) must be_==(34)
      fib(10) must be_==(55)
      fib(11) must be_==(89)
      fib(12) must be_==(144)
    }
  }

  "fibs" should {
    "give me the fib at the correct index" in {
      (1 to 15).map{ i: Int =>
        f(i) must be_==(fib(i))
      }
    }

    "give me the fib with digits" in {
      indexOfFirstWith(2) must be_==(7)
    }

    "have the first number with 1000 digits" in {
      indexOfFirstWith(1000) must be_==(4782)
    }

  }

  def indexOfFirstWith(numberOfDigits: Int): Int = {
    fibs.takeWhile(num => num.toString.size < numberOfDigits).size + 1
  }
}
