import org.specs2.mutable.Specification
import CommonHelpers._

class Problem04Spec extends Specification {

  def isPalindrome(palindromicNumber: Long):Boolean = {
    palindromicNumber.toString.reverse == palindromicNumber.toString
  }

  "palindrome detector" should {
    "detect palindromes" in {
      isPalindrome(2002) should beTrue
      isPalindrome(17071) should beTrue
      isPalindrome(41111114) should beTrue
    }

    "detect non palindromes" in {
      isPalindrome(2313) should beFalse
      isPalindrome(10) should beFalse
      isPalindrome(4559455) should beFalse
    }

  }

  def perms(a: Int, b:Int):Stream[Long] = {
    for {
      x <- countDown(a)
      y <- countDown(b)
    } yield (x*y)
  }

  "highest palindrome" should {
    "be 906609" in {
      perms(999, 999).filter(isPalindrome).sorted.reverse.head should be_==(906609)
    }
  }
}
