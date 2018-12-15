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

  def execute(reqs: Prerequisites, tasks: List[Char], pending: List[Char]): (List[Char], Prerequisites) = {
    println(s"before execute - tasks: $tasks, reqs: $reqs")
    tasks.foldLeft((pending, reqs))((z, t) => {
      val (newTasks, newReqs) = execute(z._2, t)
      ((newTasks ++ z._1).sorted, newReqs)
    })
  }

  def assignTasks(pending: List[Char], elves: List[Elf]): (List[Char], List[Elf]) = {
    val (avail, busy) = elves.partition(_.isAvail())
    val count = Math.min(pending.length, avail.length)

    def loop(n: Int, tasks: List[Char], assigned: List[Elf]): (List[Char], List[Elf]) =
      if (n == 0) (tasks, assigned ++ List.fill(avail.length - count)(Elf())) else
        loop(n - 1, tasks.tail, Elf(Some(tasks.head), taskTime(tasks.head)) :: assigned)

    val (t, e) = loop(count, pending, Nil)

    (t, e ++ busy)
  }

  def decrementTime(elves: List[Elf]): List[Elf] = {
    val (idle, busy) = elves.partition(_.isAvail())
    idle ++ busy.map(elf => elf.copy(timeRemaining = elf.timeRemaining -1 ))
  }

  def solve2(lines: List[String]): Int = {
    def loop2(pendingTasks: List[Char], reqs: Prerequisites, elves: List[Elf], elapsed: Int): Int = {

      println(s"pendingTasks: $pendingTasks, elves: $elves, reqs: $reqs")

      // Are we done?
      if(pendingTasks.isEmpty && reqs.isEmpty && elves.forall(_.isAvail())) elapsed else {

        val completedTasks = elves.filter(elf => elf.isAvail() && elf.task.isDefined).map(_.task.get)

        println(s"completedTasks: $completedTasks")

        val (newTasks, newReqs) = execute(reqs, completedTasks, pendingTasks)

        println(s"after execute - newTasks: $newTasks, newReqs: $newReqs")

        val (t, e) = assignTasks(newTasks, elves)
        assert(e.length == NUM_ELVES)

        //if(elapsed == 200) sys.exit(42)

        loop2(t, newReqs, decrementTime(e), elapsed + 1)
      }
    }

    val reqs = parse(lines)

    val (init, map) = extractExecutable(reqs)

    val (t, e) = assignTasks(init, List.fill(NUM_ELVES)(Elf()))

    loop2(t, map, e, 0)
  }


  case class Elf(task: Option[Char] = None, timeRemaining: Int = 0) {
    def isAvail(): Boolean = timeRemaining <= 0
  }


  val NUM_ELVES = 4
  val TIME_CONST = 60

  def taskTime(task: Char): Int = task - 'A' + TIME_CONST + 1


  val data = List(
    "Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin.")

  // CABDFE

  def main(args: Array[String]): Unit = {
    val file = new File("data/day7.txt")

    val input = parse(file)

    val r1 = solve1(input)

    println(s"part1: $r1")

    val r2 = solve2(input)

    println(s"part2: $r2")
  }
}
