import CommonHelpers._

object ProblemThree {
  def primeFactors(i: Long): Seq[Long] = i match {
    case 1 => Nil
    case p if isPrime(p) => Seq(p)
    case _ => {
      firstPrimeFactor(i).map(p => primeFactors(i/p) :+ p).getOrElse(Nil)
    }
  }

  def firstPrimeFactor(i: Long): Option[Long] = {
    from(2, i / 2).filter(isFactor(i)).filter(isPrime).headOption
  }
}
