package top

import java.io.File

import scala.collection._
import scala.io.Source

/*
  Build a Map with the guard IDs as the keys.
  The entries are Maps of each minute as keys and entries as a List[Date] indicating when they were asleep.
 */
object Day4 {

  sealed trait Event

  case class ShiftStart(guardId: Int, date: Date) extends Event

  case class WakeUp(minute: Int) extends Event

  case class FallAsleep(minute: Int) extends Event

  case class Date(month: Int, day: Int)

  case class Time(hour: Int, minute: Int)

  type GuardHistory = Map[Int, Map[Int, List[Date]]]

  val emptyTimeLine = Map.empty[Int, List[Date]].withDefaultValue(Nil)

  val guardHistory = Map.empty[Int, Map[Int, List[Date]]].withDefaultValue(emptyTimeLine)

  case class GuardLogEntry(date: Date, time: Time, event: Event)

  val logRegex = """\[\d{4}-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (.+)""".r

  val wakeUpRegex = """wakes up""".r

  val fallAsleepRegex = """falls asleep""".r

  val shiftStartRegex = """Guard #(\d+) begins shift""".r

  // Process the Events
  def processEvents(events: List[Event], history: GuardHistory): GuardHistory = events match {
    case Nil => history

    case head :: tail =>
      val (shift, rest) = tail.span(_ match {
        case ShiftStart(_, _) => false
        case _ => true
      })

      val (guardId, instances) = processGuardShift(head :: shift)

      println(s"guardId: $guardId, sleepIntervals: $instances")

      val map = instances.foldLeft(history(guardId))((m, t)=>{
        val (minute, date) = t
        val l = date :: m(minute)
        m.updated(minute, l)
      })

      val updated = history.updated(guardId, map)

      println(s"updated: $updated")

      processEvents(rest, updated)
  }

  // Process one guard shift. The list of Events passed represents one guard shift beginning with a ShiftStart event.
  // The returned tuples represent the sequences of minutes during which the guard was asleep
  def processGuardShift(events: List[Event]): (Int, Seq[(Int, Date)]) = events match {
    case head :: tail => head match {
      case ShiftStart(guardId, date) => guardId -> sleepIntervals(tail, date)

      case _ => sys.exit(1)
    }

    case _ => sys.exit(2)
  }

  // Processes a list of alternating FallAsleep & WakeUp events
  def sleepIntervals(events: List[Event], date: Date): Seq[(Int, Date)] = events match {
      case Nil => Nil

      case head :: tail => head match {
        case FallAsleep(from) => tail.head match {
          case WakeUp(until) => (from until until).map(_ -> date) ++ sleepIntervals(tail.tail, date)

          case _ => sys.exit(3)
        }

        case _ => sys.exit(4)
      }
    }

  // Convert one line of input to a GuardLogEntry
  def convert(line: String): GuardLogEntry = line match {
    case logRegex(mo, d, h, min, event) =>
      val (month, day, hour, minute) = (mo.toInt, d.toInt, h.toInt, min.toInt)
      val date = Date(month, day)
      val time = Time(hour, minute)
      event match {
        case shiftStartRegex(guardId) => GuardLogEntry(date, time, ShiftStart(guardId.toInt, date))
        case fallAsleepRegex(_*) => GuardLogEntry(date, time, FallAsleep(minute))
        case wakeUpRegex(_*) => GuardLogEntry(date, time, WakeUp(minute))
      }
  }

  def parse(file: File): List[Event] =
    parse(Source.fromFile(file).getLines().toList)

  // Parse input converting to a List of Events sorted chronologically
  def parse(lines: List[String]): List[Event] =
    lines.map(convert).sortBy(e => (e.date.month, e.date.day, e.time.hour, e.time.minute)).map(_.event)

  def solve(history: GuardHistory): Int = {
    def total(map: Map[Int, List[Date]]): Int =
      map.toList.map { case (_, dates) => dates.length }.sum

    def max(map: Map[Int, List[Date]]): Int =
      map.toList.map { case (minute, dates) => minute -> dates.length }.sortBy { case (_, mins) => mins }.reverse.head._1

    println(s"history: $history")

    val guardId =
      history.toList.map { case (guardId, napMap) => guardId -> total(napMap) }.sortBy { case (_, mins) => mins }.reverse.head._1

    val maxMinute = max(history(guardId))

    println(s"guard: $guardId, minute: $maxMinute")

    guardId * maxMinute
  }

  val data = List(
    "[1518-11-11 00:02] Guard #2179 begins shift",
    "[1518-11-11 00:03] falls asleep",
    "[1518-11-11 00:04] wakes up",
    "[1518-11-11 00:05] falls asleep",
    "[1518-11-11 00:06] wakes up",
    "[1518-11-12 00:02] Guard #2179 begins shift",
    "[1518-11-12 00:05] falls asleep",
    "[1518-11-12 00:06] wakes up",
    "[1518-11-12 00:07] falls asleep",
    "[1518-11-12 00:08] wakes up"
  )

  def main(args: Array[String]): Unit = {
    val events = //parse(data)
      parse(new File("data/day4.txt"))

    val history = processEvents(events, guardHistory)

    println(solve(history))
  }

}
