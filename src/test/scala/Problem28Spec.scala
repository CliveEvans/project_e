import org.specs2.mutable.Specification

class Problem28Spec extends Specification {

  /*
   Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

      21 22 23 24 25
      20  7  8  9 10
      19  6  1  2 11
      18  5  4  3 12
      17 16 15 14 13

      It can be verified that the sum of the numbers on the diagonals is 101.

      What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
  */



  object Spiraliser {
    import Direction._

    class Spiral(size:Int) {

      val cells: Array[Array[Int]] = Array.ofDim[Int](size, size)

      def cellExists(p: Point): Boolean = p.x >= 0 && p.x < size && p.y >=0 && p.y < size
      def isEmpty(p: Point):Boolean = {

        cellExists(p) && cells(p.y)(p.x) == 0
      }
      def set(p: Point, value:Int): Unit = {
        if (cellExists(p)) cells(p.y)(p.x) = value
      }

      def values: Seq[Seq[Int]] = {
        cells.map(_.toSeq).toSeq
      }

      def diagonals: Seq[Seq[Int]] = {
        val max = size - 1
        Seq(
          0.to(max).map(i => cells(i)(i)),
          0.to(max).map(i => cells(max - i)(i))
        )
      }

      def sumDiagonals: Int = {
        (diagonals.flatten.sum - 1) //we're not counting the centre value twice apparently
      }

    }


    def build(size: Int): Spiral = {
      val matrix = new Spiral(size)

      val origin = (size + 1) / 2 - 1
      val start = Point(origin, origin)

      var location = start
      var value = 1
      var direction = North

      while (value <= size * size) {
        matrix.set(location, value)
        val next = direction.next
        if(matrix.isEmpty(location.move(next))){
          direction = next
        }
        location = location.move(direction)
        value += 1
      }


      matrix
    }

  }

  // counting from top left
  object Direction extends Enumeration {
    implicit def convert(value: Value): DirectionValue = value.asInstanceOf[DirectionValue]

    protected case class DirectionValue(deltaX: Int, deltaY: Int) extends super.Val() {
      def next: DirectionValue = {
        values.find(_ > this).getOrElse(East)
      }
    }

    val East = DirectionValue(1, 0)
    val South = DirectionValue(0, 1)
    val West = DirectionValue(-1, 0)
    val North = DirectionValue(0, -1)

    case class Point(x: Int, y: Int) {
      def move(direction: DirectionValue): Point = Point(x + direction.deltaX, y + direction.deltaY)
    }

  }


  "A 1x1 spiral" should {
    "should have one cell starting at 1" in {
      Spiraliser.build(1).values must be_==(Seq(Seq(1)))
    }
  }

  "a 2x2 spiral" should {
    "have 4 cells" in {
      Spiraliser.build(2).values must be_==(
        Seq(
          Seq(1, 2),
          Seq(4, 3)
        )
      )
    }
    "have 2 diaganals" in {
      Spiraliser.build(2).diagonals must be_==(
        Seq(
          Seq(1, 3),
          Seq(4, 2)
        )
      )
    }
  }

  "a 5x5 spiral" should {
    "have 25 cells" in {
      Spiraliser.build(5).values must be_==(
        Seq(
          Seq(21, 22, 23, 24, 25),
          Seq(20, 7, 8, 9, 10),
          Seq(19, 6, 1, 2, 11),
          Seq(18, 5, 4, 3, 12),
          Seq(17, 16, 15, 14, 13)
        )
      )
    }
    "have 2 diaganals" in {
      Spiraliser.build(5).diagonals must be_==(
        Seq(
          Seq(21, 7, 1, 3, 13),
          Seq(17, 5, 1, 9, 25)
        )
      )
    }
    "have diagonals that add up" in {
      Spiraliser.build(5).sumDiagonals must be_==(101)
    }
  }

  "a 1001x1001  spiral" should {
    "have diagonals that add up" in {
      Spiraliser.build(1001).sumDiagonals must be_==(669171001)
    }
  }

  "Directions" should {
    import Direction._
    "move clockwise" in {
      Direction.values must be_==(Seq(East, South, West, North))
      East.next must be_==(South)
      South.next must be_==(West)
      West.next must be_==(North)
      North.next must be_==(East)
    }

    "allow us to move a point" in {
      Point(0, 0).move(East) must be_==(Point(1, 0))
      Point(0, 0).move(North) must be_==(Point(0, -1))
      Point(0, 0).move(West) must be_==(Point(-1, 0))
      Point(0, 0).move(South) must be_==(Point(0, 1))
    }
  }

}
