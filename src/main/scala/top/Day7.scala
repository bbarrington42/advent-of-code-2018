package top

import java.io.File

import scala.io.Source

object Day7 {

  val inputRegex = """Step ([A-Z]) must be finished before step ([A-Z]) can begin\.""".r


  def parse(line: String): (Char, Char) = line match {
    case inputRegex(state, next) => (state.toCharArray.head, next.toCharArray.head)

    case _ => sys.exit(1)
  }

  // Assemble Map of prerequisites
  def parse(lines: List[String]): Map[Char, List[Char]] = {
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

  def extractExecutable(reqs: Map[Char, List[Char]]): (List[Char], Map[Char, List[Char]]) = {
    val (f, b) = reqs.partition { case (_, l) => l.isEmpty }
    f.keys.toList -> b
  }

  // Removes the prerequisite from the Map and returns a tuple of new states that are executable and the updated Map
  def execute(reqs: Map[Char, List[Char]], state: Char): (List[Char], Map[Char, List[Char]]) = {
    val m = reqs.map { case (k, l) => k -> remove(state, l) }
    extractExecutable(m)
  }

  def solve1(lines: List[String]): String = {
    def loop(execution: List[Char], preReqs: Map[Char, List[Char]]): List[Char] = {
      println(s"execution: $execution, reqs: $preReqs")
      execution match {
        case Nil => Nil

        case head :: tail =>
          val (ex, m) = execute(preReqs, head)
          head :: loop((ex ++ tail).sorted, m)
      }
    }

    val preReqs = parse(lines)

    val (init, map) = extractExecutable(preReqs)

    loop(init.sorted, map).mkString
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
  }
}
