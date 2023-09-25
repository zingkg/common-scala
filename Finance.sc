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
    2021 -> 70 /* 7.0% */,
    2022 -> 71 /* 7.1% */)

  def floor(cents: Long, purchaseYearMonth: String): Long = {
    floor(cents, java.time.YearMonth.parse(purchaseYearMonth))
  }

  def floor(cents: Long, purchaseYearMonth: java.time.YearMonth): Long = {
    val purchaseDate = purchaseYearMonth.atDay(1)
    val oneYearAgo = java.time.LocalDate.now(java.time.ZoneId.of("America/Los_Angeles")).minusYears(1)
    val result = Iterator.iterate(purchaseDate)(_.plusYears(1))
      .takeWhile(_.isBefore(oneYearAgo))
      .foldLeft(cents) { case (centsSum, date) =>
        centsSum + (centsSum * (InflationRate(date.getYear)) / 1000)
      }
    // multiply by 5%
    result * 105 / 100
  }

  def rise(cents: Long): Long = {
    // Multiply by 20%
    cents * 120 / 100
  }

  case class Report(currentPrice: Long, maxFloorTradeDate: java.time.LocalDate, maxFloor: Long, minRise: Long) {
    override def toString: String =
      s"""current_price: $currentPrice
        |floor_trade_date: ${maxFloorTradeDate.format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd"))}
        |max_floor: $maxFloor
        |min_rise: $minRise\n""".stripMargin
  }

  case class Row(symbol: String, currentPrice: Long, tradeDate: java.time.LocalDate, purchasePrice: Long, quantity: String, heldLong: Boolean)

  def parseCents(price: String): Long = {
    if (price.nonEmpty) {
      val tokens = price.split("\\.")
      val dollars = tokens(0).toLong
      val cents = if (tokens.length == 2 && tokens(1).length > 2)
        tokens(1).toLong / 10
      else if (tokens.length == 2 && tokens(1).length == 2)
        tokens(1).toLong
      else if (tokens.length == 2 && tokens(1).length == 1)
        tokens(1).toLong * 10
      else
        0

      dollars * 100 + cents
    } else {
      0
    }
  }

  def readYahooFile(filename: String, longOnly: Boolean = true): Map[String, Report] = {
    val bufferedSource = scala.io.Source.fromFile(filename)
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val rows = lines.drop(1).map(_.split(","))
      .filter(tokens => tokens(0).nonEmpty && tokens.length > 11 && (!longOnly || tokens.length >= 15))
      .map { tokens =>
        Row(
          tokens(0).toLowerCase,
          parseCents(tokens(1)),
          java.time.LocalDate.parse(tokens(9), java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd")),
          parseCents(tokens(10)),
          tokens(11),
          tokens.length >= 15 && tokens(15).nonEmpty
        )
      }
      .toVector

    rows.groupBy(_.symbol)
      .map { case (symbol, rows) =>
        val rowsHeldLong = rows.filter(row => !row.quantity.startsWith("0.") && (!longOnly || row.heldLong))
        val currentPrice = rows(0).currentPrice
        val floors = rowsHeldLong
          .map(row => (row.tradeDate, floor(row.purchasePrice, java.time.YearMonth.from(row.tradeDate))))
          .filter(_._2 < currentPrice)
        val rises = rowsHeldLong
          .map(row => rise(row.purchasePrice))
          .filter(_ > currentPrice)
        val maxFloor = if (floors.nonEmpty) floors.maxBy(_._2) else (java.time.LocalDate.of(1970, 1, 1), 0L)
        val minRise = if (rises.nonEmpty) rises.min else 0
        symbol -> Report(currentPrice, maxFloor._1, maxFloor._2, minRise)
      }
  }
}

import Finance._