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
            if (Seq(java.time.DayOfWeek.SATURDAY, java.time.DayOfWeek.SUNDAY).contains(date.getDayOfWeek))
              Seq(
                s"${date.format(printer)} morning",
                s"${date.format(printer)} afternoon",
                s"${date.format(printer)} evening")
            else
              Seq(s"${date.format(printer)} evening")
          }
          .toList)
    }
  }

  def buildPoll(question: String, from: String, to: String): String = {
    val maybeDates = buildDates(from, to)
    maybeDates.map { dates =>
      val groupString = dates.grouped(20)
        .toList
        .map { dateGroup =>
          val choices = Iterator.iterate('a'.toInt)(_ + 1)
            .takeWhile(_ <= 't'.toInt)
            .toList
            .zip(dateGroup)
            .map { case (numeral, choice) =>
                s"choice_${numeral.toChar}: $choice"
            }
          s"/poll question: $question ${choices.mkString(" ")}"
        }
        .toList
      groupString.mkString("\n")
    }.getOrElse("From must be before until")
  }

  def printPoll(question: String, from: String, to: String): Unit = {
    println(buildPoll(question, from, to))
  }
}

import SimplePoll._
