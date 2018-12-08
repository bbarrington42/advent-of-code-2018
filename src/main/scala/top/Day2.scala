package top

import java.io.File

import scala.io.Source

object Day2 {

  def letterCounts(letters: List[Char]): Set[Int] = {
    def loop(ls: List[Char]): Set[Int] = ls match {
      case Nil => Set.empty

      case head :: tail => Set(letters.filter(_ == head).length) ++ loop(tail)
    }

    loop(letters)
  }

  def solve1(strings: List[String]): Int = {

    val sets = strings.map(s => letterCounts(s.toCharArray.toList))

    val twos = sets.filter(_.contains(2)).length

    val threes = sets.filter(_.contains(3)).length

    twos * threes
  }

  def removeAt(i: Int, ls: Array[Char]): Array[Char] = {
    val (front, back) = ls.splitAt(i)
    front ++ back.tail
  }

  def solve2(ids: List[String]): Option[String] = {

    def search(ls: List[String]): Option[String] = ls match {
      case Nil => None

      case head :: tail => tail.find(_ == head).fold(search(tail))(Option(_))
    }

    def loop(idx: List[Int]): Option[String] = idx match {
      case Nil => None

      case head :: tail =>
        val ss = ids.map(id => removeAt(head, id.toCharArray)).map(chars => new String(chars))
        search(ss).fold(loop(tail))(Option(_))
    }

    val range = 0 until ids.head.length
    loop(range.toList)
  }


  def parse(file: File): List[String] =
    Source.fromFile(file).getLines().toList

  def main(args: Array[String]): Unit = {
    val file = new File("data/day2.txt")

    val ids = parse(file)

    val r1 = solve1(ids)

    println(s"part 1: $r1")

    val r2 = solve2(ids)

    println(s"part 2: ${r2.getOrElse("unknown")}")
  }
}
