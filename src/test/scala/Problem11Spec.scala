import org.specs2.mutable.Specification

class Problem11Spec extends Specification {


  val realGrid = List(
    List(8, 2, 22, 97, 38, 15, 0, 40, 0, 75, 4, 5, 7, 78, 52, 12, 50, 77, 91, 8),
    List(49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 4, 56, 62, 0),
    List(81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 3, 49, 13, 36, 65),
    List(52, 70, 95, 23, 4, 60, 11, 42, 69, 24, 68, 56, 1, 32, 56, 71, 37, 2, 36, 91),
    List(22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80),
    List(24, 47, 32, 60, 99, 3, 45, 2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50),
    List(32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70),
    List(67, 26, 20, 68, 2, 62, 12, 20, 95, 63, 94, 39, 63, 8, 40, 91, 66, 49, 94, 21),
    List(24, 55, 58, 5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72),
    List(21, 36, 23, 9, 75, 0, 76, 44, 20, 45, 35, 14, 0, 61, 33, 97, 34, 31, 33, 95),
    List(78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 3, 80, 4, 62, 16, 14, 9, 53, 56, 92),
    List(16, 39, 5, 42, 96, 35, 31, 47, 55, 58, 88, 24, 0, 17, 54, 24, 36, 29, 85, 57),
    List(86, 56, 0, 48, 35, 71, 89, 7, 5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58),
    List(19, 80, 81, 68, 5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 4, 89, 55, 40),
    List(4, 52, 8, 83, 97, 35, 99, 16, 7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66),
    List(88, 36, 68, 87, 57, 62, 20, 72, 3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69),
    List(4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 8, 46, 29, 32, 40, 62, 76, 36),
    List(20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 4, 36, 16),
    List(20, 73, 35, 29, 78, 31, 90, 1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 5, 54),
    List(1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 1, 89, 19, 67, 48)
  )

  def transpose[A](m: Matrix[A]): Matrix[A] = {
    if (m.head.isEmpty) Nil
    else m.map(_.head) :: transpose(m.map(_.tail))
  }

  def horizontals[A](grid: Matrix[A], size: Int): Seq[Seq[A]] = {
    grid.map(r => {
      for (start <- 0 to (r.size - size)) yield r.slice(start, start + size)
    }).flatMap(_.map(identity))
  }

  def verticals[A](grid: Matrix[A], size: Int): Seq[Seq[A]] = {
    horizontals(transpose(grid), size)
  }

  def backSlash[A](grid: Matrix[A], size: Int): Seq[Seq[A]] = {
    val flattened = grid.flatten
    val rowLength = grid.head.size
    for {
      startX <- 0 to (rowLength - size)
      startY <- 0 to (grid.size - size)
    } yield {
      val initial = startX + (startY * rowLength)
      (0 to (size - 1)).map(offset => flattened(initial + (offset * (rowLength + 1))))
    }
  }

  def forwardSlash[A](grid: Matrix[A], size: Int): Seq[Seq[A]] = {
    backSlash(grid.reverse, size)
  }

  type Row[A] = List[A]
  type Matrix[A] = List[Row[A]]

  def largestProduct(grid: Matrix[Int], size: Int): Long = {
    (horizontals(grid, size) ++ verticals(grid, size) ++ backSlash(grid, size) ++ forwardSlash(grid, size))
      .map(_.product)
      .max
  }

  "transponse" should {
    "do what it says" in {
      transpose(testGrid) should beEqualTo(Seq(
        Seq(21, 92, 72, 7, 15),
        Seq(44, 5, 39, 35, 29),
        Seq(11, 33, 51, 61, 60),
        Seq(8, 12, 7, 22, 79),
        Seq(12, 74, 24, 95, 11)
      ))
    }
  }

  "horizontals" should {
    "be '((length + 1) - size) * depth' long" in {
      horizontals(testGrid, 4).flatten should haveSize(40)
      horizontals(testGrid, 3).flatten should haveSize(45)
      horizontals(testGrid, 2).flatten should haveSize(40)
    }
    "contain all the numbers" in {
      horizontals(testGrid, 2) should contain(Seq(21, 44), Seq(5, 33), Seq(79, 11))
    }
  }

  "verticals" should {
    "be '((length + 1) - size) * depth' long" in {
      verticals(testGrid, 4).flatten should haveSize(40)
      verticals(testGrid, 3).flatten should haveSize(45)
      verticals(testGrid, 2).flatten should haveSize(40)
    }
    "contain all the numbers" in {
      verticals(testGrid, 4) should contain(Seq(21, 92, 72, 7), Seq(5, 39, 35, 29), Seq(74, 24, 95, 11))
    }
  }

  "slashes" should {
    "contain expected numbers when backslashing" in {
      backSlash(indexGrid, 4) should contain(Seq(0, 6, 12, 18), Seq(1, 7, 13, 19), Seq(5, 11, 17, 23), Seq(6, 12, 18, 24))
    }
    "contain expected numbers when backslashing" in {
      backSlash(testGrid, 4) should contain(Seq(21, 5, 51, 22), Seq(92, 39, 61, 79), Seq(44, 33, 7, 95), Seq(5, 51, 22, 11))
    }
    "contain expected numbers when forward slashing" in {
      forwardSlash(indexGrid, 4) should contain(Seq(15, 11, 7, 3), Seq(16, 12, 8, 4), Seq(20, 16, 12, 8), Seq(21, 17, 13, 9))
    }
  }

  "largest product" should {
    "be 92 * 39 * 61 * 79 if size 4 with testGrid" in {
      largestProduct(testGrid, 4) should be_==(92 * 39 * 61 * 79)
    }
    "be really big in the real one" in {
      largestProduct(realGrid, 4) should be_==(70600674)
    }
  }

  val testGrid = List(
    List(21, 44, 11, 8, 12),
    List(92, 5, 33, 12, 74),
    List(72, 39, 51, 7, 24),
    List(7, 35, 61, 22, 95),
    List(15, 29, 60, 79, 11)
  )

  val indexGrid = List(
    List(0, 1, 2, 3, 4),
    List(5, 6, 7, 8, 9),
    List(10, 11, 12, 13, 14),
    List(15, 16, 17, 18, 19),
    List(20, 21, 22, 23, 24)
  )
}
