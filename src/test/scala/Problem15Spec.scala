import org.specs2.mutable.Specification
import scala.collection.mutable

class Problem15Spec extends Specification {

  /*
  Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

  rrdd
  rdrd
  rddr
  drrd
  drdr
  ddrr
  

  How many such routes are there through a 20×20 grid?
   */

  // Brute force some routes to prove
  def routes(gridSize: Int): Seq[String] = {
    def getSteps(remainingDowns: Int, remainingRights: Int): Seq[String] = {
      from.getOrElseUpdate((remainingDowns, remainingRights), {

        if (remainingDowns == 0 && remainingRights == 0) {
          Nil
        } else if (remainingDowns == 0) {
          getSteps(remainingDowns, remainingRights - 1).map("r" + _)
        } else if (remainingRights == 0) {
          getSteps(remainingDowns - 1, remainingRights).map("d" + _)
        } else {
          val right = getSteps(remainingDowns, remainingRights - 1).map("r" + _)
          val down = getSteps(remainingDowns - 1, remainingRights).map("d" + _)
          right ++ down
        }
      })
      from((remainingDowns, remainingRights))
    }
    getSteps(gridSize, gridSize)
  }

  val from: mutable.Map[(Int, Int), Seq[String]] = {
    mutable.Map((0, 0) -> Seq(), (0, 1) -> Seq("r"), (1, 0) -> Seq("d"))
  }

  // The combinator formula is right
  def routeCount(gridSize:Int) = {
    def choose(n: BigInt, k: BigInt): BigInt = {
      val numerator = ((k + 1) to n).product
      val denominator = (BigInt(1) to k).product
      numerator/denominator
    }
    choose(gridSize*2, gridSize)
  }

  "a 1 x 1 grid" should {
    "have 2 steps for all routes" in {
      routes(1) must contain(haveSize[String](2)).forall
    }
    "give the correct routes" in {
      routes(1) must containTheSameElementsAs(Seq("rd", "dr"))
    }
  }

  "a 2 x 2 grid" should {
    "have 4 steps for all routes" in {
      routes(2) must contain(haveSize[String](4)).forall
    }
    "have 6 routes" in {
      routes(2) must haveSize(6)
      routeCount(2) must be_==(6)
    }
    "the right routes" in {
      routes(2) must containTheSameElementsAs(Seq(
        "rrdd",
        "rdrd",
        "rddr",
        "drrd",
        "ddrr",
        "drdr"
      ))
    }
  }

  "a 3 x 3 grid" should {
    val routesFor3 = routes(3)
    "have 6 steps for all routes" in {
      routesFor3 must contain(haveSize[String](6)).forall
    }
    "have 18 routes" in {
      routesFor3 must haveSize(20)
      routeCount(3) must be_==(20)
    }
    "the right routes" in {
      routesFor3 must containTheSameElementsAs(Seq(
        "dddrrr", "ddrdrr", "ddrrdr", "ddrrrd", "drddrr", "drdrdr", "drdrrd", "drrddr", "drrdrd", "drrrdd", "rdddrr", "rddrdr", "rddrrd", "rdrddr", "rdrdrd", "rdrrdd", "rrdddr", "rrddrd", "rrdrdd", "rrrddd"
      ))
    }
  }

  "a 5 x 5 grid" should {
    "have the same nubmer of routes as the combinator thinks" in {
      routes(5).size must be_==(routeCount(5))
    }
  }

  "a 20 x 20 grid" should {

    "have lots of routes" in {
      routeCount(20) must be_==(137846528820L)
    }
  }

}
