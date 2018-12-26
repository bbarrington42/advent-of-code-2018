package top

import java.io.File

import scala.io.Source

object Day10 {
  // position=< 21373,  53216> velocity=<-2, -5>,
  val pointRegex =
    """position=<([^,]+),([^>]+)> velocity=<([^,]+),([^>]+)>""".r

  case class Point(px: Int, py: Int, vx: Int, vy: Int)

  def parse(line: String): Point = line match {
    case pointRegex(px, py, vx, vy) => Point(px.trim.toInt, py.trim.toInt, vx.trim.toInt, vy.trim.toInt)
  }

  def parse(lines: List[String]): List[Point] = lines.map(parse)

  def parse(file: File): List[Point] = {
    val lines = Source.fromFile(file).getLines().toList
    parse(lines)
  }


  def view(width: Int, height: Int, points: List[Point]): String = {
    // Create an array first
    val array = Array.fill[Char](height, width)('.')
    val updated = points.foldLeft(array)((z, p) => {
      if (p.px >= 0 && p.px < width && p.py >= 0 && p.py < height)
        z.updated(p.py, z(p.py).updated(p.px, '#')) else z
    })
    show(updated)
  }

  def show(display: Array[Array[Char]]): String =
    display.map(chars => chars.mkString).mkString("\n")

  def tick(seconds: Int, points: List[Point]): List[Point] = {
    points.map { case Point(px, py, vx, vy) => Point(px + vx * seconds, py + vy * seconds, vx, vy) }
  }


  val data = List(
    "position=< 9,  1> velocity=< 0,  2>",
    "position=< 7,  0> velocity=<-1,  0>",
    "position=< 3, -2> velocity=<-1,  1>",
    "position=< 6,  10> velocity=<-2, -1>",
    "position=< 2, -4> velocity=< 2,  2>",
    "position=<-6,  10> velocity=< 2, -2>",
    "position=< 1,  8> velocity=< 1, -1>",
    "position=< 1,  7> velocity=< 1,  0>",
    "position=<-3,  11> velocity=< 1, -2>",
    "position=< 7,  6> velocity=<-1, -1>",
    "position=<-2,  3> velocity=< 1,  0>",
    "position=<-4,  3> velocity=< 2,  0>",
    "position=<10, -3> velocity=<-1,  1>",
    "position=< 5,  11> velocity=< 1, -2>",
    "position=< 4,  7> velocity=< 0, -1>",
    "position=< 8, -2> velocity=< 0,  1>",
    "position=<15,  0> velocity=<-2,  0>",
    "position=< 1,  6> velocity=< 1,  0>",
    "position=< 8,  9> velocity=< 0, -1>",
    "position=< 3,  3> velocity=<-1,  1>",
    "position=< 0,  5> velocity=< 0, -1>",
    "position=<-2,  2> velocity=< 2,  0>",
    "position=< 5, -2> velocity=< 1,  2>",
    "position=< 1,  4> velocity=< 2,  1>",
    "position=<-2,  7> velocity=< 2, -2>",
    "position=< 3,  6> velocity=<-1, -1>",
    "position=< 5,  0> velocity=< 1,  0>",
    "position=<-6,  0> velocity=< 2,  0>",
    "position=< 5,  9> velocity=< 1, -2>",
    "position=<14,  7> velocity=<-2,  0>",
    "position=<-3,  6> velocity=< 2, -1>")


  def main(args: Array[String]): Unit = {
    val file = new File("data/day10.txt")

    val width = 22
    val height = 16

    val testPoints = List(
      Point(0, 0, 0, 0),
      Point(width - 1, 0, 0, 0),
      Point(width - 1, height - 1, 0, 0),
      Point(0, height - 1, 0, 0))


    val ZERO_OFFSET = (width / 4, height / 4)
    val points = parse(data).map { case Point(px, py, vx, vy) => Point(px + ZERO_OFFSET._1, py + ZERO_OFFSET._2, vx, vy) }


    val display = view(width, height, tick(3, points))

    print(display)
  }
}



