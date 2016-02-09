import org.specs2.mutable.Specification

class Problem24Spec extends Specification {

  /*
    A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the 
    digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it 
    lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
    
    012   021   102   120   201   210
    
    What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
   */


  def perms(numbers: Seq[Int]):Stream[Seq[Int]] = {
    def permutations(n: Stream[Int]): Stream[Seq[Int]] = n match {
      case Stream.Empty => Stream.empty
      case s if s.length <= 1 => Stream(s)
      case _ =>
        n.flatMap(s =>
          perms(n.diff(List(s))).map(s +: _)
        )
    }
    permutations(numbers.sorted.toStream)
  }

  "perms" should {
    "stop on empty string" in {
      perms(Nil) must beEmpty
    }
    "match a trivial example" in {
      perms(Seq(1)) must be_==(Seq(Seq(1)))
    }

    "match two numbers" in {
      perms(Seq(2,1)) must be_==(Seq(Seq(1,2), Seq(2,1)))
    }

    "match the example provided" in {
      perms(Seq(0,1,2)) must be_==(Seq(Seq(0, 1, 2), Seq(0, 2, 1), Seq(1, 0, 2), Seq(1, 2, 0), Seq(2, 0, 1), Seq(2, 1, 0)))
    }

    "be able to fetch a very large number in sensible time" in {
      perms(Seq(1,2,3,4,5,6,7,8,9,0)).drop(999999).head must be_==(Seq(2, 7, 8, 3, 9, 1, 5, 4, 6, 0))
    }
  }
}
