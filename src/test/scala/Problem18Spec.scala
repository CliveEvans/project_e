import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class Problem18Spec extends Specification with ScalaCheck {

  /*
  By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

        3
       7 4
      2 4 6
     8 5 9 3

  That is, 3 + 7 + 4 + 9 = 23.

  Find the maximum total from top to bottom of the triangle below:

                  75
                 95 64
                17 47 82
               18 35 87 10
              20 04 82 47 65
             19 01 23 75 03 34
            88 02 77 73 07 63 67
           99 65 04 28 06 16 70 92
          41 41 26 56 83 40 80 70 33
         41 48 72 33 47 32 37 16 94 29
        53 71 44 65 25 43 91 52 97 51 14
       70 11 33 28 77 73 17 78 39 68 17 57
      91 71 52 38 17 14 91 43 58 50 27 29 48
     63 66 04 68 89 53 67 30 73 16 69 87 40 31
    04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

  NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route.
  However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot be solved by brute force, and requires a clever method! ;o)

   */

  import Node._
  import Treebuilder.buildFrom

  "an empty node" should {
    "have a best cost of 0" in {
      Node.empty.largestTotal must be_==(0)
    }
  }

  "a node of one value" should {
    "have a best cost of that value" in {
      prop((x: Long) =>
        terminatingNode(x).largestTotal must be_==(x)
      )
    }
  }

  "a node" should {
    "have the highest cost of it's elements if it's value is zero" in {
      prop { (left: Long, right: Long) =>
        val leftNode = terminatingNode(left)
        val rightNode = terminatingNode(right)
        new Populated(0, leftNode, rightNode).largestTotal must be_==(Math.max(left, right))
      }
    }
    "have the add it's value to the cost of it's elements" in {
      prop { (self: Long, left: Long, right: Long) =>
        val leftNode = terminatingNode(left)
        val rightNode = terminatingNode(right)
        new Populated(self, leftNode, rightNode).largestTotal must be_==(self + Math.max(left, right))
      }
    }
  }

  "treebuilder" should {
    "create a single node succesfully" in {
      buildFrom("21") must be_==(terminatingNode(21))
    }
    "create a simple(!) tree" in {
      val tree = """
          |21
          |47 53
        """.stripMargin

      buildFrom(tree) must be_==(
        new Populated(21, terminatingNode(47), terminatingNode(53))
      )
    }

    "can cope with an more complex tree" in {
      val tree =
        """
          |       7
          |     16 5
          |   11 24 3
          | 22 23 17 99
        """.stripMargin

      val n22 = terminatingNode(22)
      val n23 = terminatingNode(23)
      val n17 = terminatingNode(17)
      val n99 = terminatingNode(99)
      val n11 = new Populated(11, n22, n23)
      val n24 = new Populated(24, n23, n17)
      val n3  = new Populated(3, n17, n99)
      val n16 = new Populated(16, n11, n24)
      val n5  = new Populated(5, n24, n3)
      val n7  = new Populated(7, n16, n5)

      buildFrom(tree) must be_==(n7)
    }
  }

  "a complex tree" should {
    "have the expected highest cost" in {
      val tree = """
                   |        3
                   |       7 4
                   |      2 4 6
                   |     8 5 9 3
                   |     """.stripMargin

      buildFrom(tree).largestTotal must be_==(23)
    }
  }

  "the actual problem" should {
    "be the correct answer " in {
      val largeTree =
        """
          |                   75
          |                 95 64
          |                17 47 82
          |               18 35 87 10
          |              20 04 82 47 65
          |             19 01 23 75 03 34
          |            88 02 77 73 07 63 67
          |           99 65 04 28 06 16 70 92
          |          41 41 26 56 83 40 80 70 33
          |         41 48 72 33 47 32 37 16 94 29
          |        53 71 44 65 25 43 91 52 97 51 14
          |       70 11 33 28 77 73 17 78 39 68 17 57
          |      91 71 52 38 17 14 91 43 58 50 27 29 48
          |     63 66 04 68 89 53 67 30 73 16 69 87 40 31
          |    04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
        """.stripMargin

      buildFrom(largeTree).largestTotal must be_==(1074)
    }
  }

}


object Node {

  def terminatingNode(element: Long): Node = {
    new Populated(element, empty, empty)
  }

  val empty:Node = new Empty
}
abstract class Node {
  def isEmpty: Boolean

  val largestTotal: Long
}

class Empty extends Node {
  def isEmpty: Boolean = true

  val largestTotal = 0L

  def canEqual(other: Any): Boolean = other.isInstanceOf[Node]

  override def equals(other: Any): Boolean = other match {
    case that: Node => that.isEmpty
    case _ => false
  }

  override def hashCode(): Int = 77

  override def toString = "Empty"

}

case class Populated(val elem: Long, val left: Node, val right: Node) extends Node {
  def isEmpty: Boolean = false

  val largestTotal = elem + Math.max(left.largestTotal, right.largestTotal)

}

object Treebuilder {
  import Node._

  def buildFrom(s: String):Node = {
    def nodes(elements: Seq[Long], children: Seq[(Node, Node)]): Seq[Node] = {
      elements.zip(children).map{ case (element, (left, right)) => new Populated(element, left, right)}
    }

    def readLine(l: String): List[Long] = {
      l.split("""\s+""").map(_.toLong).toList
    }
    val lines:Seq[Seq[Long]] = s.split("""\n""").map(_.trim).filter(_.nonEmpty).reverse.map(readLine)

    val lastLine: Seq[Node] = lines.head.map(new Populated(_, empty, empty))

    lines.tail.foldLeft(lastLine){case (previousLine, thisLine) =>
      val children: Seq[(Node, Node)] = previousLine.sliding(2).collect{
        case List(left, right) => (left, right)
        case List(single) => (single, empty)
      }.toList
      nodes(thisLine, children)
    }.head

  }
}