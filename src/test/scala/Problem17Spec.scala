import org.specs2.mutable.Specification

class Problem17Spec extends Specification {

  /*
  If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

  If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


  NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
   */

  "NumberAsWord" >> {
    "single digits numbers" should {
      "be simple" in {
        NumberAsWord(1) must be_==("one")
        NumberAsWord(2) must be_==("two")
        NumberAsWord(3) must be_==("three")
        NumberAsWord(4) must be_==("four")
        NumberAsWord(5) must be_==("five")
        NumberAsWord(6) must be_==("six")
        NumberAsWord(7) must be_==("seven")
        NumberAsWord(8) must be_==("eight")
        NumberAsWord(9) must be_==("nine")
      }
    }

    "factors of ten below 100" should {
      "be equally simple" in {
        NumberAsWord(10) must be_==("ten")
        NumberAsWord(20) must be_==("twenty")
        NumberAsWord(30) must be_==("thirty")
        NumberAsWord(40) must be_==("forty")
        NumberAsWord(50) must be_==("fifty")
        NumberAsWord(60) must be_==("sixty")
        NumberAsWord(70) must be_==("seventy")
        NumberAsWord(80) must be_==("eighty")
        NumberAsWord(90) must be_==("ninety")
      }
    }

    "simple double digit numbers" should {
      "be reatively simple" in {
        NumberAsWord(25) must be_==("twenty-five")
        NumberAsWord(47) must be_==("forty-seven")
      }
    }

    "teens" should {
      "be handled" in {
        NumberAsWord(11) must be_==("eleven")
        NumberAsWord(12) must be_==("twelve")
        NumberAsWord(13) must be_==("thirteen")
        NumberAsWord(14) must be_==("fourteen")
        NumberAsWord(15) must be_==("fifteen")
        NumberAsWord(16) must be_==("sixteen")
        NumberAsWord(17) must be_==("seventeen")
        NumberAsWord(18) must be_==("eighteen")
        NumberAsWord(19) must be_==("nineteen")
      }
    }

    "three digit numbers" should {
      "be relatively straighforward" in {
        NumberAsWord(127) must be_==("one hundred and twenty-seven")
        NumberAsWord(596) must be_==("five hundred and ninety-six")
      }
    }

    "multiples of 100" should {
      "work" in {
        NumberAsWord(200) must be_==("two hundred")
      }
    }

    "1000" should {
      "be the largest number we care about" in {
        NumberAsWord(1000) must be_==("one thousand")
      }
    }
  }

  "the numbers one to 5" should {
    "have nineteen letters in total" in {
      countLettersForRange(1 to 5) must be_==(19)
    }
  }

  "when counting" should {
    "ignore hyphens and spaces" in {
      countLetters(342) must be_==(23)
      countLetters(115) must be_==(20)
    }
  }

  "From 1 to 1 thousand" should {
    "be quite a lot of letters" in {
      countLettersForRange(1 to 1000) must be_==(21124)
    }
  }

  val stopChars = Set(' ', '-')

  def countLetters(n: Int):Long = {
    NumberAsWord(n).filterNot(stopChars.contains).length
  }

  def countLettersForRange(range: Range): Long = {
    range.map(countLetters).sum
  }
}
