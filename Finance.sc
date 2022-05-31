object Finance {
  // Source: https://www.thebalance.com/u-s-inflation-rate-history-by-year-and-forecast-3306093
  // Basis point of 1 decimal point, e.g. 1% = 10
  private val InflationRate: Map[Int, Int] = Map(
    2014 -> 8 /* 0.8% */,
    2015 -> 7 /* 0.7% */,
    2016 -> 21 /* 2.1% */,
    2017 -> 21 /* 2.1% */,
    2018 -> 19 /* 1.9% */,
    2019 -> 23 /* 2.3% */,
    2020 -> 14 /* 1.4% */,
    2021 -> 70 /* 7.0% */)

  def floor(cents: Long, purchaseDateString: String): Long = {
    val purchaseDate = java.time.LocalDate.parse(purchaseDateString, java.time.format.DateTimeFormatter.ISO_LOCAL_DATE)
    val oneYearAgo = java.time.LocalDate.now(java.time.ZoneId.of("America/New_York")).minusYears(1)
    val result = Iterator.iterate(purchaseDate)(_.plusYears(1))
      .takeWhile(_.isBefore(oneYearAgo))
      .foldLeft(cents) { case (centsSum, date) =>
        centsSum + (centsSum * (InflationRate(date.getYear)) / 1000)
      }
    result * 101 / 100
  }
}

import Finance._
