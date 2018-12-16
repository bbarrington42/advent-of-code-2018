package top

import java.io.File

import scala.io.Source

object Day7 {

  type Prerequisites = Map[Char, List[Char]]

  val inputRegex = """Step ([A-Z]) must be finished before step ([A-Z]) can begin\.""".r


  def parse(line: String): (Char, Char) = line match {
    case inputRegex(state, next) => (state.toCharArray.head, next.toCharArray.head)

    case _ => sys.exit(1)
  }

  // Assemble Map of prerequisites
  def parse(lines: List[String]): Prerequisites = {
    def unique(tuples: List[(Char, Char)]): List[Char] =
      tuples.foldLeft(Set.empty[Char])((z, t) => z + t._1 + t._2).toList

    val tuples = lines.map(parse)
    // Build the initial map. All states with no prerequisites.
    val m = unique(tuples).foldLeft(Map.empty[Char, List[Char]])((z, c) => z.updated(c, Nil))
    tuples.foldLeft(m)((z, t) => z.updated(t._2, t._1 :: z(t._2)))
  }

  def remove[A](a: A, ls: List[A]): List[A] = {
    val n = ls.indexOf(a)
    if (n == -1) ls else {
      val (front, back) = ls.splitAt(n)
      front ++ back.drop(1)
    }
  }

  def parse(file: File): List[String] =
    Source.fromFile(file).getLines().toList

  def extractExecutable(reqs: Prerequisites): (List[Char], Prerequisites) = {
    val (f, b) = reqs.partition { case (_, l) => l.isEmpty }
    f.keys.toList.sorted -> b
  }

  // Removes the prerequisite from the Map and returns a tuple of new sorted states that are executable along with the updated Map
  def execute(reqs: Prerequisites, state: Char): (List[Char], Prerequisites) = {
    val m = reqs.map { case (k, l) => k -> remove(state, l) }
    extractExecutable(m)
  }

  def solve1(lines: List[String]): String = {
    def loop(execution: List[Char], preReqs: Prerequisites): List[Char] = execution match {
      case Nil => Nil

      case head :: tail =>
        val (ex, m) = execute(preReqs, head)
        head :: loop((ex ++ tail).sorted, m)
    }


    val preReqs = parse(lines)

    val (init, map) = extractExecutable(preReqs)

    loop(init, map).mkString
  }


  //// Specific to Part 2 /////////////

  val NUM_ELVES = 4
  val TIME_CONST = 60


  case class Elf(task: Option[Char] = None, timeRemaining: Int = 0) {
    def isAvail(): Boolean = timeRemaining <= 0
  }

  def taskTime(task: Char): Int = task - 'A' + TIME_CONST + 1

  // Execute each pending task. This adjusts the state of prerequisites and adds more pending tasks.
  def execute(reqs: Prerequisites, tasks: List[Char], pending: List[Char]): (List[Char], Prerequisites) = {
    val (t, r) = tasks.foldLeft((pending, reqs))({ case ((p, r), t) =>
      val (newTasks, newReqs) = execute(r, t)
      ((newTasks ++ p), newReqs)
    })
    (t.sorted, r)
  }

  // Assign as many tasks as possible to the idle elves.
  def assignTasks(tasks: List[Char], elves: List[Elf]): (List[Char], List[Elf]) = {
    def loop(_tasks: List[Char], _elves: List[Elf], accElves: List[Elf]): (List[Char], List[Elf]) = (_tasks, _elves) match {
      case (Nil, _) | (_, Nil) => (_tasks, _elves ++ accElves)

      case (th :: ttail, _ :: etail) =>
        loop(ttail, etail, Elf(Some(th), taskTime(th)) :: accElves)
    }

    val (idle, busy) = elves.partition(_.isAvail())

    loop(tasks, idle, busy)
  }


  def decrementTime(elves: List[Elf]): List[Elf] = {
    val (idle, busy) = elves.partition(_.isAvail())
    idle ++ busy.map(elf => elf.copy(timeRemaining = elf.timeRemaining - 1))
  }

  def solve2(lines: List[String]): Int = {
    def loop(pendingTasks: List[Char], reqs: Prerequisites, elves: List[Elf], elapsed: Int): Int = {

      // Are we done?
      if (pendingTasks.isEmpty && reqs.isEmpty && elves.forall(_.isAvail())) elapsed else {

        val completedTasks = elves.filter(elf => elf.isAvail() && elf.task.isDefined).map(_.task.get)

        val (newTasks, newReqs) = execute(reqs, completedTasks, pendingTasks)

        val (t, e) = assignTasks(newTasks, elves)

        loop(t, newReqs, decrementTime(e), elapsed + 1)
      }
    }

    val reqs = parse(lines)

    val (init, map) = extractExecutable(reqs)

    val (t, e) = assignTasks(init, List.fill(NUM_ELVES)(Elf()))

    loop(t, map, e, 0)
  }


  val data = List(
    "Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin.")

  def main(args: Array[String]): Unit = {
    val file = new File("data/day7.txt")

    val input = parse(file)

    val r1 = solve1(input)

    println(s"part1: $r1")

    val r2 = solve2(input)

    println(s"part2: $r2")
  }
}
