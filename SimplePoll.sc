/**
 * Interfaces with Discord's simple poll feature to create polls expediently
 */
object SimplePoll {
  def buildDates(from: String, until: String): scala.util.Try[Seq[String]] = {
    val formatter = java.time.format.DateTimeFormatter.ISO_LOCAL_DATE
    val printer = java.time.format.DateTimeFormatter.ofPattern("MM/dd EEEE")
    val fromDate = java.time.LocalDate.parse(from, formatter)
    val untilDate = java.time.LocalDate.parse(until, formatter)
    if (fromDate.isAfter(untilDate)) {
      scala.util.Failure(new IllegalArgumentException("From must be before until"))
    } else {
      scala.util.Success(
        Iterator.iterate(fromDate)(_.plusDays(1))
          .takeWhile(_.isBefore(untilDate))
          .flatMap { date =>
            val dateFormatted = date.format(printer)
            if (Seq(java.time.DayOfWeek.SATURDAY, java.time.DayOfWeek.SUNDAY).contains(date.getDayOfWeek))
              Seq(
                s"$dateFormatted morning",
                s"$dateFormatted afternoon",
                s"$dateFormatted evening")
            else
              Seq(s"$dateFormatted evening")
          }
          .toList)
    }
  }

  def buildRange(question: String, from: String, to: String, withEnd: Boolean = true): String = {
    val maxSize = 20
    val maybeDates = buildDates(from, to)
    maybeDates.map { dates =>
      val groupString = (dates :+ ":x: None of these dates/times work").grouped(20)
        .toList
        .map(buildPoll(question, _))
        .toList
      groupString.mkString("\n")
    }.getOrElse("From must be before until")
  }

  private def buildPoll(question: String, choiceGroup: Seq[String]): String = {
    val choices = Iterator.iterate('a'.toInt)(_ + 1)
      .takeWhile(_ <= 't'.toInt)
      .toList
      .zip(choiceGroup)
      .map { case (numeral, choice) =>
          s"choice_${numeral.toChar}: $choice"
      }
    s"/poll question: $question ${choices.mkString(" ")}"
  }

  def pRange(question: String, from: String, to: String): Unit = {
    println(buildRange(question, from, to))
  }

  def single(question: String, dateString: String): String = {
    val formatter = java.time.format.DateTimeFormatter.ISO_LOCAL_DATE
    val printer = java.time.format.DateTimeFormatter.ofPattern("MM/dd EEEE")
    val dateFormatted = java.time.LocalDate.parse(dateString, formatter).format(printer)
    val choices = Seq(
      s":sunrise: $dateFormatted morning",
      s":sunny: $dateFormatted afternoon",
      s":bridge_at_night: $dateFormatted evening") ++
      confirm

    buildPoll(question, choices)
  }

  def pSingle(question: String, dateString: String): Unit = {
    println(single(question, dateString))
  }

  val confirm: Seq[String] = {
    Seq(
      ":speaking_head: Yes, I will be in person",
      ":desktop: Yes, I will be connecting remotely",
      ":x: No, I will not be present")
  }

  def buildConfirm(question: String): String = {
    buildPoll(question, confirm)
  }

  def pConfirm(question: String): Unit = {
    println(buildConfirm(question))
  }
}

import SimplePoll._
