import java.time.DayOfWeek
import java.time.DayOfWeek._

import org.joda.time.DateTime
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification

class Problem19Spec extends Specification {

  /*
  You are given the following information, but you may prefer to do some research for yourself.

  1 Jan 1900 was a Monday.
  Thirty days has September,
  April, June and November.
  All the rest have thirty-one,
  Saving February alone,
  Which has twenty-eight, rain or shine.
  And on leap years, twenty-nine.
  A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

  How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
  */

  val firstOf1900: DateTime = DateTime.parse("1900-01-01")

  def beA(dayOfWeek: DayOfWeek): Matcher[DateTime] = {
    d: DateTime => (d.getDayOfWeek == dayOfWeek.getValue, s"$d is not a $dayOfWeek")
  }

  implicit class EnrichedDateStream(days: Stream[DateTime]) {
    def only(dayOfWeek: DayOfWeek): Stream[DateTime] = days.filter(_.getDayOfWeek == dayOfWeek.getValue)
  }


  "a stream of firsts" should {
    "all be the 1st" in {
      firstsFrom(firstOf1900).take(10) must contain(isFirst).forall
    }

    "all be sundays if filtered" in {
      val dateTimes: Stream[DateTime] = firstsFrom(firstOf1900).only(SUNDAY).take(10)
      dateTimes must contain(beA(SUNDAY)).forall
    }
  }

  val twentiethCenturyFirsts = firstsFrom(DateTime.parse("1901-01-01")).takeWhile(_.getYear < 2001)

  implicit val orderingByDateTime: Ordering[DateTime] = Ordering.by(_.getMillis)

  "the 20th century" should {
    "apparently run from 1 Jan 1901 to 31 Dec 2000" in {
      twentiethCenturyFirsts must contain(be_<(DateTime.parse("2001-01-01"))).forall
    }

    "contain some number of sundays" in {
      twentiethCenturyFirsts.only(SUNDAY) must haveSize(171)
    }
  }

  "1900-01-01" should {
    "be a Monday" in {
      firstOf1900 must beA(MONDAY)
    }
  }

  def isFirst: Matcher[DateTime] = {
    date: DateTime => (date.getDayOfMonth == 1, s"$date is not the first of the month, it is the ${date.getDayOfMonth} day")
  }

  def firstsFrom(startDate: DateTime): Stream[DateTime] = {
    startDate #:: firstsFrom(startDate.plusMonths(1))
  }

}
