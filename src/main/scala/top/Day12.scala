package top

import java.io.File

import scala.io.Source

object Day12 {

  val stateRegex = """initial state: ([.#]+)""".r

  val ruleRegex = """([.#]{5}) => ([.#])""".r

  case class Rule(pattern: String, result: Char)

  def toRule(line: String): Option[Rule] = line match {
    case ruleRegex(pattern, result) => Option(Rule(pattern, result.toCharArray()(0)))
    case _ => None
  }

  def toState(line: String): Array[Char] = line match {
    case stateRegex(state) => state.toCharArray
    case _ => throw new Exception()
  }

  def parse(lines: List[String]): (Array[Char], Seq[Rule]) = lines match {
    case head :: tail =>
      (toState(head), tail.map(toRule).filter(_.nonEmpty).map(_.get))

    case _ => throw new Exception()
  }

  def parse(file: File): (Array[Char], Seq[Rule]) = {
    val lines = Source.fromFile(file).getLines().toList
    parse(lines)
  }

  // Extracts the String describing to plant being examined
  def context(n: Int, state: Array[Char]): String =
    state.slice(n - 2, n + 3).mkString


  // The next state of the current plant
  def nextState(n: Int, state: Array[Char], rules: Seq[Rule]): Char = {
    val ctx = context(n, state)
    rules.find(rule => rule.pattern == ctx).get.result
  }

  def nextGeneration(state: Array[Char], rules: Seq[Rule]): Array[Char] = {
    def loop(n: Int, state: Array[Char]): Array[Char] = {
      if (n > state.length - 3) state else
        loop(n + 1, state.updated(n, nextState(n, state, rules)))
    }

    // Prepend and append 2 empty pots
    val empties = Array('.', '.')
    loop(2, empties ++ state ++ empties)
  }

  def part1(generations: Int, state: Array[Char], rules: Seq[Rule]): Int = {
    def result(state: Array[Char]): Int = {
      1
    }

    def loop(gen: Int, state: Array[Char]): Int = {
      if(gen == 0) result(state) else {
        val nextGen = nextGeneration(state, rules)
        println(nextGen.mkString)
        loop(gen - 1, nextGen)
      }
    }

    loop(generations, state)
  }


  def main(args: Array[String]): Unit = {
    val file = new File("data/day12.txt")

    val (state, rules) = parse(file)

    val r = part1(20, state, rules)
  }
}
