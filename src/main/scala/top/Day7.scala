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
    lines.foldLeft(Map.empty[Char, List[Char]].withDefaultValue(Nil))((z, line) => {
      val (preReq, state) = parse(line)
      z.updated(state, preReq :: z(state))
    })
  }

  def remove[A](a: A, ls: List[A]): List[A] = {
    val (front, back) = ls.span(_ != a)
    front ++ back.tail
  }

  def parse(file: File): Map[Char, List[Char]] =
    parse(Source.fromFile(file).getLines().toList)

  // Determine the initial states. An initial state must not have any prerequisites.
  def init(map: Map[Char, List[Char]]): List[Char] =
    map.filter { case (state, preReqs) => preReqs.isEmpty }.toList.map { case (state, _) => state }.sorted

  def solve1(lines: List[String]): String = {
    def loop(preReqs: Map[Char, List[Char]], states: List[Char]): List[Char] = states match {
      case Nil => Nil

      case _ =>
        val (front, back) = states.span(ch => preReqs(ch).nonEmpty)
        val updated = preReqs.map { case (ch, ls) => ch -> remove(back.head, ls) }
        back.head :: loop(updated, (front ++ back.tail).sorted)
    }

    val prs = parse(lines)
    loop(prs, init(prs)).mkString
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

    val r1 = solve1(data)

    println(s"part1: $r1")
  }
}
