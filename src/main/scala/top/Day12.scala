package top

import java.io.File

import scala.io.Source

object Day12 {

  val stateRegex = """initial state: ([.#]+)""".r

  val ruleRegex = """([.#]{5}) => ([.#])""".r

  case class Rule(pattern: String, result: Char)

  case class State(zero: Int, plants: Array[Char]) {
    override def toString(): String = f"State($zero%2d, ${plants.mkString}"
  }

  def toRule(line: String): Option[Rule] = line match {
    case ruleRegex(pattern, result) => Option(Rule(pattern, result.toCharArray()(0)))
    case _ => None
  }

  def toState(line: String): State = line match {
    case stateRegex(plants) => State(0, plants.toCharArray)
    case _ => throw new Exception()
  }

  def parse(lines: List[String]): (State, Seq[Rule]) = lines match {
    case head :: tail =>
      (toState(head), tail.map(toRule).filter(_.nonEmpty).map(_.get))

    case _ => throw new Exception()
  }

  def parse(file: File): (State, Seq[Rule]) = {
    val lines = Source.fromFile(file).getLines().toList
    parse(lines)
  }

  // Extracts the String describing the plant being examined
  def context(n: Int, state: State): String =
    state.plants.slice(n - 2, n + 3).mkString


  // The next state of the current plant
  def nextState(n: Int, state: State, rules: Seq[Rule]): Char = {
    val ctx = context(n, state)
    rules.find(_.pattern == ctx).map(_.result).getOrElse('.')
  }

  def nextGeneration(state: State, rules: Seq[Rule]): State = {
    val empty = Array.fill(2)('.')
    val prev = State(state.zero + 2, empty ++ state.plants ++ empty)

    def loop(n: Int, state: State): State = {
      if (n > state.plants.length - 3) state else
        loop(n + 1, State(state.zero, state.plants.updated(n, nextState(n, prev, rules))))
    }

    loop(2, prev)
  }

  def part1(generations: Int, state: State, rules: Seq[Rule]): (List[State], Int) = {
    def result(state: State): Int =
      state.plants.foldLeft((0, 0))({ case ((i, r), c) => if (c == '#') {
        //println(s"r: $r, zero: ${state.zero}, i: $i")
        (i + 1, r - state.zero + i)
      } else (i + 1, r)
      })._2


    def loop(gen: Int, states: List[State]): (List[State], Int) = {
      if (gen == 0) (states, result(states.head)) else
        loop(gen - 1, nextGeneration(states.head, rules) :: states)

    }

    loop(generations, List(state))
  }

  def pad(len: Int)(state: State): State = {
    val x = len - state.plants.length
    val p = Array.fill(x/2)('.')
    state.copy(plants = p ++ state.plants ++ p)
  }

  def main(args: Array[String]): Unit = {
    val file = new File("data/day12.txt")

    val (state, rules) = parse(file)

    val r = part1(20, state, rules)

    // Pad shorter entries
    val len = r._1.maxBy(_.plants.length).plants.length
    val x = r._1.map(pad(len)).reverse

    println(x.mkString("\n"))
    println(r._2)
  }
}
