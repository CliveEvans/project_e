object NumberAsWord {
  val smallNumberWords = Map(
    1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five",
    6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine", 10 -> "ten",
    11 -> "eleven", 12 -> "twelve", 13 -> "thirteen", 14 -> "fourteen", 15 -> "fifteen",
    16 -> "sixteen", 17 -> "seventeen", 18 -> "eighteen", 19 -> "nineteen"
  )

  val tensWords = Map(
    2 -> "twenty",
    3 -> "thirty",
    4 -> "forty",
    5 -> "fifty",
    6 -> "sixty",
    7 -> "seventy",
    8 -> "eighty",
    9 -> "ninety"
  )


  def apply(number: Int): String = number match {
    case n if n == 1000 => "one thousand"
    case n if n > 99 =>
      smallNumberWords(n/100) + " hundred" + {
        if (n % 100 != 0)  " and " + NumberAsWord(n%100)
        else ""
      }
    case n if n < 20 => smallNumberWords(n)
    case n if n % 10 == 0 => tensWords(n / 10)
    case n => tensWords(n / 10) + "-" + smallNumberWords(n % 10)
  }
}