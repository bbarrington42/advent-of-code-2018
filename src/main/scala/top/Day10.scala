package top

import java.io.File

import scala.io.{Source, StdIn}

object Day10 {
  // position=< 21373,  53216> velocity=<-2, -5>,
  val pointRegex =
    """position=<([^,]+),([^>]+)> velocity=<([^,]+),([^>]+)>""".r

  case class Point(x: Long, y: Long, vx: Long, vy: Long)

  case class Intercept(time: Long, coordinate: Long)

  def xform(line: String): Point = line match {
    case pointRegex(px, py, vx, vy) => Point(px.trim.toInt, py.trim.toInt, vx.trim.toInt, vy.trim.toInt)
  }

  def parse(file: File): List[Point] = {
    val lines = Source.fromFile(file).getLines().toList
    parse(lines)
  }

  def parse(lines: List[String]): List[Point] =
    lines.map(xform)


  // For a given set of points, create a view to show them
  def view(width: Long, height: Long, points: List[Point]): String = {
    // Create an array first
    val array = Array.fill[Char](height.toInt, width.toInt)('.')
    // Update the visible points
    val updated = points.foldLeft(array)((z, p) =>
      if (p.x >= 0 && p.x < width && p.y >= 0 && p.y < height)
        z.updated(p.y.toInt, z(p.y.toInt).updated(p.x.toInt, '#')) else z
    )
    updated.map(chars => chars.mkString).mkString("\n")
  }

  def update(points: List[Point], seconds: Long): List[Point] =
    points.map { case Point(px, py, vx, vy) => Point(px + vx * seconds.toLong, py + vy * seconds.toLong, vx, vy) }

  // Calculate the sum of the absolute values of each Point coordinate. Determine the time at which this is a minimum.
  def magicTime(points: List[Point], initTime: Long): Long = {
    def entropy(time: Long, points: List[Point]): Long =
      update(points, time).foldLeft(0L)((z, p) => z + Math.abs(p.x) + Math.abs(p.y))

    def loop(time: Long, sum: Long = 0): Long = {
      val newSum = entropy(time, points)
      if (newSum > sum) time else loop(time + 1, newSum)
    }

    loop(initTime, entropy(initTime, points))
  }


  // Enter loop to control display with keyboard
  def keyLoop(width: Long, height: Long, points: List[Point], initTime: Long): Unit = {
    def readChar(): Option[Char] = try {
      Option(StdIn.readChar())
    } catch {
      case _: Throwable => None
    }

    def loop(points: List[Point], time: Long): Unit = {
      println()
      println(s"time: $time")
      println(view(width, height, update(points, time)))

      readChar() match {
        case Some(k) => k match {
          case 'z' => loop(points, time - 1)
          case 'x' => loop(points, time + 1)
          case 'q' | 'Q' =>

          case _ => loop(points, time)
        }

        case None => loop(points, time)
      }
    }

    loop(points, initTime)
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


  // 10144
  // GGLZLHCE

  def main(args: Array[String]): Unit = {

    val file = new File("data/day10.txt")

    val points = parse(file)

    val time = magicTime(points, 0)

    println(time)

    val updated = update(points, time)

    val top = updated.minBy(_.y).y
    val bottom = updated.maxBy(_.y).y
    val left = updated.minBy(_.x).x
    val right = updated.maxBy(_.x).x

    println(s"top: $top, left: $left, bottom: $bottom, right: $right")

    val width = right - left
    val height = bottom - top
    println(s"width: $width, height: $height")

    keyLoop(width, height, points, time)
  }
}



