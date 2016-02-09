import org.specs2.mutable.Specification

import scala.io.Source

class Problem22Spec extends Specification {

  /*

    Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names,
    begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value
    by its alphabetical position in the list to obtain a name score.

    For example, when the list is sorted into alphabetical order,
    COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
    So, COLIN would obtain a score of 938 × 53 = 49714.

    What is the total of all the name scores in the file?
  */

  "score for a string" should {
    "give 0 for an empty string" in {
      valueOf("") must be_==(0)
    }
    "give 1 for a" in {
      valueOf("a") must be_==(1)
    }
    "give 53 for COLIN" in {
      valueOf("colin") must be_==(53)
    }
  }

  val quotedNames: Iterator[String] = Source.fromInputStream(getClass.getResourceAsStream("p022_names.txt")).getLines().flatMap(_.split(","))
  val names = quotedNames.map(_.replaceAll("\"", "")).toList.sorted

  "reading the file" should {
    "find colin in 938the position" in {
      names(937) must be_==("COLIN")
    }
  }

  "score" should {
    "give the value for a list with a single element" in {
      score(Seq("C")) must be_==(3)
    }

    "multiply the second element by 2" in {
      score(Seq("", "C")) must be_==(6)
    }

    "give us a nice big number for the file" in {
      score(names) must be_==(871198282)
    }
  }


  def valueOf(name: String): BigInt = {
    name.toUpperCase.toCharArray.map(_ - 'A' + 1).sum
  }

  def score(names: Seq[String]): BigInt = {
    names.map(valueOf).zipWithIndex.map {
      case (score, index) => (index + 1) * score
    }.sum
  }
}
