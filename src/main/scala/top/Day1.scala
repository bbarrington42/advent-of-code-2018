package top

import java.io.File

import scala.io.Source

object Day1 {

  def parse(lines: List[String]): List[Int] =
    lines.map(_.toInt)

  def parse(file: File): List[Int] =
    parse(Source.fromFile(file).getLines().toList)

  def run(changes: List[Int]): Int =
    changes.foldLeft(0)((z, i) => z + i)

  def run2(changes: List[Int]): Int = {
    def loop(l: List[Int], map: Map[Int, Int], curr: Int): Int = l match {
      case Nil => loop(changes, map, curr)

      case head :: tail =>
        val v = map(curr)
        if(v >= 1) curr else loop(tail, map.updated(curr, v + 1), curr + head)
    }

    loop(changes, Map.empty[Int, Int].withDefaultValue(0), 0)
  }

  def main(args: Array[String]): Unit = {
    val file = new File("data/day1.txt")

    println(run2(parse(file)))
  }
}
