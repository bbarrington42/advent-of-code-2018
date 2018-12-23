package top

import java.io.File

import scala.io.Source

object Day10 {
  // position=< 21373,  53216> velocity=<-2, -5>
  val pointRegex =
    """position=<([- ]\d+), ([- ]\d+)> velocity=<([- ]\d+), ([- ]\d+)>""".r

  case class Point(px: Int, py: Int, vx: Int, vy: Int)

  def parse(line: String): Point = line match {
    case pointRegex(px, py, vx, vy) => Point(px.trim.toInt, py.trim.toInt, vx.trim.toInt, vy.trim.toInt)
  }

  def parse(lines: List[String]): List[Point] = lines.map(parse)

  def parse(file: File): List[Point] = {
    val lines = Source.fromFile(file).getLines().toList
    parse(lines)
  }


  def main(args: Array[String]): Unit = {
    val file = new File("data/day10.txt")

    val r = parse(file)

    println(r)
  }
}



