import org.specs2.mutable.Specification

import scala.io.Source

class Problem67Spec extends Specification {

  import Triangle.Trianglebuilder

  "the example" should {
    "still work" in {
      val triangle =
        """
          |   3
          |  7 4
          | 2 4 6
          |8 5 9 3
        """.stripMargin

      Trianglebuilder.from(triangle).largestTotal must be_==(23)
    }
  }

  "the huge file" should {
    "give the largest total quickly enough" in {
      val lines: Iterator[String] = Source.fromURL(getClass.getResource("p067_triangle.txt")).getLines()
      lines must not(beEmpty)
      Trianglebuilder.from(lines.toSeq).largestTotal must be_==(7273)
    }
  }

}
