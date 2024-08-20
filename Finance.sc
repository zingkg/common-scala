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
    2022 -> 65 /* 6.5% */,
    2023 -> 27 /* 2.7% */)

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
    // multiply by 12%
    result * 112 / 100
  }

  def rise(cents: Long): Long = {
    // Multiply by 20%
    cents * 120 / 100
  }

  def sRise(cents: Long): Long = {
    // Multiply by 5%
    cents * 105 / 100
  }

  case class Floor(tradeDate: java.time.LocalDate, floor: Long, low: Long, isFifo: Boolean) {
    override def toString: String =
      s"""trade_date: ${tradeDate.format(java.time.format.DateTimeFormatter.ofPattern("MM/dd/yyyy"))}; floor: $floor$additionalInfo""".stripMargin

    private def additionalInfo: String = {
      val floorAboveLow = floor > low
      if (isFifo && floorAboveLow)
        s" F***"
      else if (floorAboveLow)
        " ***"
      else if (isFifo)
        " F"
      else
        ""
    }
  }

  case class Report(currentPrice: Long, low: Long, high: Long, floors: Seq[Floor], rises: Seq[Long]) {
    override def toString: String =
      s"""current_price: $currentPrice; low: $low; high: $high
        |rises: ${rises.mkString(", ")}
        |floors: ${floors.mkString(",\n")}\n""".stripMargin
  }

  case class Row(symbol: String, currentPrice: Long, low: Long, high: Long, tradeDate: java.time.LocalDate, purchasePrice: Long, quantity: String, heldLong: Boolean)

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

  def readYahooFile(filename: String, longOnly: Boolean = true, unrestrictedFloor: Boolean = true): Map[String, Report] = {
    val bufferedSource = scala.io.Source.fromFile(filename)
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val rows = lines.drop(1).map(_.split(","))
      .filter(tokens => tokens(0).nonEmpty && tokens.length > 11 && (!longOnly || tokens.length >= 15))
      .map { tokens =>
        Row(
          symbol = tokens(0).toLowerCase,
          currentPrice = parseCents(tokens(1)),
          low = parseCents(tokens(7)),
          high = parseCents(tokens(6)),
          tradeDate = java.time.LocalDate.parse(tokens(9), java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd")),
          purchasePrice = parseCents(tokens(10)),
          quantity = tokens(11),
          heldLong = tokens.length > 15 && tokens(15).nonEmpty
        )
      }
      .toVector

    rows.groupBy(_.symbol)
      .map { case (symbol, rows) =>
        val rowsHeldLong = rows.filter(row => !row.quantity.startsWith("0.") && (!longOnly || row.heldLong))
        val firstRow = rows.sortBy(row => java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd").format(row.tradeDate)).headOption.map(_.tradeDate)
        val currentPrice = rows(0).currentPrice
        val low = rows(0).low
        val high = rows(0).high
        val rises = rowsHeldLong
          .map(row => sRise(floor(row.purchasePrice, java.time.YearMonth.from(row.tradeDate))))
          .filter(_ > currentPrice)
          .sorted

        val underFloors = rowsHeldLong
          .map(row => Floor(row.tradeDate, floor(row.purchasePrice, java.time.YearMonth.from(row.tradeDate)), row.low, checkFifo(row, firstRow)))
          .filter(_.floor < currentPrice)
        val sortedUnderFloors = if (underFloors.nonEmpty) underFloors.sortBy(_.floor)(Ordering[Long].reverse).take(5) else Seq.empty
        val sortedFloors = if (unrestrictedFloor) {
          val overFloors = rowsHeldLong
            .map(row => Floor(row.tradeDate, floor(row.purchasePrice, java.time.YearMonth.from(row.tradeDate)), row.low, checkFifo(row, firstRow)))
            .filter(_.floor >= currentPrice)
            .sortBy(_.floor)
            .take(3)
            .reverse
          overFloors ++ sortedUnderFloors
        } else {
          sortedUnderFloors
        }
        symbol -> Report(currentPrice, low, high, sortedFloors, rises.take(5))
      }
  }

  private def checkFifo(row: Row, firstRowDate: Option[java.time.LocalDate]): Boolean = {
    firstRowDate.exists(_ == row.tradeDate)
  }
}
