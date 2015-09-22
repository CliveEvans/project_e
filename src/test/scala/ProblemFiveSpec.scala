import org.specs2.mutable.Specification

import scala.collection.immutable.Range.Inclusive

class ProblemFiveSpec extends Specification {

  def divisibleBy(range: Inclusive): (Int) => Boolean = i => {
    ! range.exists( i % _ != 0)
  }

  "2520" should {
    "be the lowest number evenly divisible by 1 to 10" in {
      Stream.from(1).filter(divisibleBy(1 to 10)).head should be_==(2520)
    }
  }

  "from 1 to 20" should {
    "be a bigger number" in {
      Stream.from(1).filter(divisibleBy(1 to 20)).head should be_==(232792560)
    }
  }
}
