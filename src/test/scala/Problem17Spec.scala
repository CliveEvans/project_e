import org.specs2.mutable.Specification

class Problem17Spec extends Specification {

  /*
  If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

  If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


  NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
   */


  val smallNumberWords = Map(
    1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five",
    6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine", 10 -> "ten",
    11 -> "eleven", 12 -> "twelve", 13 -> "thirteen", 14 -> "fourteen", 15 -> "fifteen",
    16 -> "sixteen", 17 -> "seventeen", 18 -> "eighteen", 19 -> "nineteen"
  )

  val tensWords = Map(
    2 -> "twenty",
    3 -> "thirty",
    4 -> "forty",
    5 -> "fifty",
    6 -> "sixty",
    7 -> "seventy",
    8 -> "eighty",
    9 -> "ninety"
  )


  def asWord(number: Int): String = number match {
    case n if n < 20 => smallNumberWords(n)
    case n if n % 10 == 0 => tensWords(n / 10)
    case n => tensWords(n / 10) + "-" + smallNumberWords(n % 10)
  }

  "single digits numbers" should {
    "be simple" in {
      asWord(1) must be_==("one")
      asWord(2) must be_==("two")
      asWord(3) must be_==("three")
      asWord(4) must be_==("four")
      asWord(5) must be_==("five")
      asWord(6) must be_==("six")
      asWord(7) must be_==("seven")
      asWord(8) must be_==("eight")
      asWord(9) must be_==("nine")
    }
  }

  "factors of ten below 100" should {
    "be equally simple" in {
      asWord(10) must be_==("ten")
      asWord(20) must be_==("twenty")
      asWord(30) must be_==("thirty")
      asWord(40) must be_==("forty")
      asWord(50) must be_==("fifty")
      asWord(60) must be_==("sixty")
      asWord(70) must be_==("seventy")
      asWord(80) must be_==("eighty")
      asWord(90) must be_==("ninety")
    }
  }

  "simple double digit numbers" should {
    "be reatively simple" in {
      asWord(25) must be_==("twenty-five")
      asWord(47) must be_==("forty-seven")
    }
  }

  "teens" should {
    "be handled" in {
      asWord(11) must be_==("eleven")
      asWord(12) must be_==("twelve")
      asWord(13) must be_==("thirteen")
      asWord(14) must be_==("fourteen")
      asWord(15) must be_==("fifteen")
      asWord(16) must be_==("sixteen")
      asWord(17) must be_==("seventeen")
      asWord(18) must be_==("eighteen")
      asWord(19) must be_==("nineteen")
    }
  }

  "three digit numbers" should {
    "be relatively straighforward" in {
      asWord(127) must be_==("one hundred and twenty-seven")
    }
  }

}
