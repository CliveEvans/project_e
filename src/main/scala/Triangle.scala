object Triangle {

  object Node {

    def terminatingNode(element: Long): Node = {
      new Populated(element, empty, empty)
    }

    val empty: Node = new Empty
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

  object Trianglebuilder {
    import Node._

    def from(s: String): Node = {
      from(s.split("""\n"""))
    }

    def from(lines: Seq[String]): Node = {

      def nodes(elements: Seq[Long], children: Seq[(Node, Node)]): Seq[Node] = {
        elements.zip(children).map { case (element, (left, right)) => new Populated(element, left, right) }
      }

      def readLine(l: String): List[Long] = {
        l.split("""\s+""").map(_.toLong).toList
      }
      val reversed: Seq[Seq[Long]] = lines.map(_.trim).filter(_.nonEmpty).reverse.map(readLine)

      val lastLine: Seq[Node] = reversed.head.map(new Populated(_, empty, empty))

      reversed.tail.foldLeft(lastLine) { case (previousLine, thisLine) =>
        val children: Seq[(Node, Node)] = previousLine.sliding(2).collect {
          case List(left, right) => (left, right)
          case List(single) => (single, empty)
        }.toList
        nodes(thisLine, children)
      }.head

    }
  }

}
