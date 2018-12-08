package top

import java.io.File

import scala.io.Source

object Day5 {

  def annihilate(front: List[Char], back: List[Char]): (List[Char], List[Char]) = back match {
    case Nil => front -> back

    case head :: tail => front.headOption.fold(front -> back)(char =>
      if (isReactive(char, head)) annihilate(front.tail, tail) else front -> back)
  }

  def isReactive(l: Char, r: Char): Boolean =
    if (l == r) false else if (l.toLower == r) true else if (l == r.toLower) true else false

  def solve1(chars: List[Char]): Int = {

    def loop(front: List[Char], back: List[Char]): Int = back match {
      case Nil => front.length

      case head :: tail =>
        val (f, b) = annihilate(head :: front, tail)
        loop(f, b)
    }

    loop(Nil, chars)
  }

  def solve2(chars: List[Char]): Int = {
    val polymers = for {
      char <- 'a' to 'z'
      l = chars.filterNot(c => c == char || c.toLower == char)
    } yield l

    polymers.map(polymer => solve1(polymer)).sortBy(identity).head
  }

  def parse(file: File): List[Char] =
    Source.fromFile(file).getLines().toList.foldLeft(List.empty[Char])((z, s) => z ++ s.toCharArray)


  def main(args: Array[String]): Unit = {
    val file = new File("data/day5.txt")

    val chars = parse(file)

    val r1 = solve1(chars)

    println(s"part 1: $r1")

    val r2 = solve2(chars)

    println(s"part 2: $r2")
  }
}
