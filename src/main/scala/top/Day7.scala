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

  /* Here is the new approach
  1 - bookkeeping for completed tasks - previously developed 'execute' method can be used
  2 - if the elves are all idle and the task list is empty, we're done
  2 - take the elves that just completed their task and assign new tasks to them - this should remove the tasks from the pending list
  3 - loop
  */


  case class Elf(task: Option[Char] = None, timeRemaining: Int = 0) {
    def isAvail(): Boolean = timeRemaining <= 0
  }

  def execute(reqs: Prerequisites, elves: List[Elf], pending: List[Char]): (List[Char], Prerequisites) = {
    elves.filter(_.isAvail()).foldLeft(pending -> reqs)((z, elf) =>
      elf.task.fold(pending -> reqs)(task => {
        val (states, newReqs) = execute(reqs, task)
        (remove(task, z._1) ++ states).sorted -> newReqs
      })
    )
  }

  // Assign tasks to the available elves, returns any remaining tasks and the elves
  def assignTasks(pending: List[Char], avail: Int): (List[Char], List[Elf]) = {
    def loop(count: Int, remaining: List[Char], assigned: List[Elf]): (List[Char], List[Elf]) =
      if (count == 0) (remaining, assigned) else
        loop(count - 1, remaining.tail,
          Elf(Some(remaining.head), taskTime(remaining.head)) :: assigned)


    val (tasks, elves) = loop(Math.min(avail, pending.length), pending, Nil)
    val n = avail - elves.length
    val idle = if (n > 0) List.fill(n)(Elf()) else Nil
    (tasks, elves ++ idle)
  }

  val NUM_ELVES = 4
  val TIME_CONST = 60

  def taskTime(task: Char): Int = task - 'A' + TIME_CONST + 1

  def solve2(lines: List[String]): Int = {
    def loop(pending: List[Char], prerequites: Prerequisites, elves: List[Elf], elapsed: Int): Int = {

      println(s"pending: $pending, elves: $elves, prerequisites: $prerequites")
      // Execute any completed tasks. The returned tasks are those available for assignment.
      val (tasks, reqs) = execute(prerequites, elves, pending)

      if (tasks.isEmpty && elves.forall(_.isAvail())) elapsed else {
        //println(s"tasks: $tasks, reqs: $reqs")
        // Assign tasks to the free elves
        val (done, occupied) = elves.partition(_.isAvail())

        val (remaining, curr) = assignTasks(tasks, done.length)

        //println(s"remaining: $remaining, curr: $curr")
        // Adjust times
        loop(remaining, reqs, (curr ++ occupied).map(elf => elf.copy(timeRemaining = elf.timeRemaining - 1)), elapsed + 1)
      }
    }

    val reqs = parse(lines)

    val (init, map) = extractExecutable(reqs)

    loop(init, map, List.fill(NUM_ELVES)(Elf()), 0)
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

    val input = parse(file)

    val r1 = solve1(input)

    println(s"part1: $r1")

    val r2 = solve2(input)

    println(s"part2: $r2")
  }
}
