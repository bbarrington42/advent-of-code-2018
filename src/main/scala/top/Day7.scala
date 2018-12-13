package top

import java.io.File

import scala.io.Source

object Day7 {

  type Prerequites = Map[Char, List[Char]]

  val inputRegex = """Step ([A-Z]) must be finished before step ([A-Z]) can begin\.""".r


  def parse(line: String): (Char, Char) = line match {
    case inputRegex(state, next) => (state.toCharArray.head, next.toCharArray.head)

    case _ => sys.exit(1)
  }

  // Assemble Map of prerequisites
  def parse(lines: List[String]): Prerequites = {
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

  def extractExecutable(reqs: Prerequites): (List[Char], Prerequites) = {
    val (f, b) = reqs.partition { case (_, l) => l.isEmpty }
    f.keys.toList.sorted -> b
  }

  // Removes the prerequisite from the Map and returns a tuple of new states that are executable along with the updated Map
  def execute(reqs: Prerequites, state: Char): (List[Char], Prerequites) = {
    val m = reqs.map { case (k, l) => k -> remove(state, l) }
    extractExecutable(m)
  }

  def solve1(lines: List[String]): String = {
    def loop(execution: List[Char], preReqs: Prerequites): List[Char] = execution match {
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
  case class Elf(task: Option[Char] = None, timeRemaining: Int = 0) {
    def isAvail(): Boolean = timeRemaining == 0
  }

  def execute(reqs: Prerequites, elves: List[Elf], execution: List[Char]): (List[Char], Prerequites) = {
    elves.foldLeft(execution -> reqs)((z, elf) =>
      elf.task.fold(execution -> reqs)(task => {
        val (states, newReqs) = execute(reqs, task)
        (remove(task, z._1) ++ states).sorted -> newReqs
      })
    )
  }

  val TIME_CONST = 60

  def taskTime(task: Char): Int = task - 'A' + TIME_CONST + 1

  def solve2(lines: List[String]): Int = {
    def loop(execution: List[Char], prerequites: Prerequites, elves: List[Elf], elapsed: Int): Int = execution match {
      case Nil => elapsed

      case _ =>
        // Look for free Elves
        val (free, occupied) = elves.partition(_.isAvail())
        //println(s"free: $free, occupied: $occupied")
        // Execute any completed tasks
        val (tasks, reqs) = execute(prerequites, free, execution)
        //println(s"tasks: $tasks, reqs: $reqs")
        // Assign tasks to the free elves
        // todo Handle case where there are more free Elves than tasks
        val assigned = free.zip(tasks).map { case (_, task) => Elf(Option(task), taskTime(task)) }
        //println(s"assigned: $assigned")
        // Adjust times
        loop(tasks, reqs, free ++ (assigned ++ occupied).map(elf => elf.copy(timeRemaining = elf.timeRemaining - 1)), elapsed + 1)
    }

    val reqs = parse(lines)

    val (init, map) = extractExecutable(reqs)

    loop(init, map, List.fill(4)(Elf()), 0)
  }

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

    val r1 = solve1(parse(file))

    println(s"part1: $r1")

    val r2 = solve2(parse(file))

    println(s"part2: $r2")
  }
}
