package top

import java.io.File

import scala.io.{Source, StdIn}

object Day10 {
  // position=< 21373,  53216> velocity=<-2, -5>,
  val regex =
    """position=<([^,]+),([^>]+)> velocity=<([^,]+),([^>]+)>""".r

  case class Point(x: Int, y: Int)

  case class Velocity(vx: Int, vy: Int)

  case class BoundingBox(topLeft: Point, bottomRight: Point)

  case class LightRay(point: Point, velocity: Velocity)

  def xform(line: String): LightRay = line match {
    case regex(x, y, vx, vy) =>
      LightRay(Point(x.trim.toInt, y.trim.toInt), Velocity(vx.trim.toInt, vy.trim.toInt))
  }

  def parse(file: File): List[LightRay] = {
    val lines = Source.fromFile(file).getLines().toList
    parse(lines)
  }

  def parse(lines: List[String]): List[LightRay] = lines.map(xform)

  // Returns topLeft, bottomRight
  def boundingBox(points: List[Point]): BoundingBox = {
    val top = points.minBy(_.y).y
    val left = points.minBy(_.x).x
    val bottom = points.maxBy(_.y).y
    val right = points.maxBy(_.x).x
    BoundingBox(Point(left, top), Point(right, bottom))
  }

  // returns (width, height)
  def dimensions(boundingBox: BoundingBox): (Int, Int) =
    (boundingBox.bottomRight.x - boundingBox.topLeft.x + 1, boundingBox.bottomRight.y - boundingBox.topLeft.y + 1)

  // the offset needed to display topLeft at the origin of the view
  def offset(boundingBox: BoundingBox): Point =
    Point(-boundingBox.topLeft.x, -boundingBox.topLeft.y)

  // For a given set of points, create a view to show them
  def view(rays: List[LightRay]): String = {
    val points = rays.map { case LightRay(point, _) => point }
    val box = boundingBox(points)
    val (width, height) = dimensions(box)
    // Create an array to hold all the points. Default char '.'
    val array = Array.fill[Char](height.toInt, width.toInt)('.')
    // Update the visible points
    val os = offset(box)
    val updated = points.map { case Point(x, y) => Point(x + os.x, y + os.y) }.foldLeft(array)((z, p) =>
      if (p.x >= 0 && p.x < width && p.y >= 0 && p.y < height)
        z.updated(p.y, z(p.y).updated(p.x, '#')) else z
    )
    updated.map(chars => chars.mkString).mkString("\n")
  }

  def update(points: List[LightRay], seconds: Int): List[LightRay] =
    points.map { case LightRay(Point(x, y), Velocity(vx, vy)) =>
      LightRay(Point(x + vx * seconds, y + vy * seconds), Velocity(vx, vy))
    }

  // Calculate the sum of the absolute values of each Point coordinate. Determine the time at which this is a minimum.
  def magicTime(points: List[LightRay], initTime: Int): Int = {
    def entropy(time: Int, points: List[LightRay]): Long =
      update(points, time).foldLeft(0)((z, p) => z + Math.abs(p.point.x) + Math.abs(p.point.y))

    def loop(time: Int, sum: Long = 0): Int = {
      val newSum = entropy(time, points)
      if (newSum > sum) time else loop(time + 1, newSum)
    }

    loop(initTime, entropy(initTime, points))
  }


  // Enter loop to control display with keyboard
  def keyLoop(points: List[LightRay], initTime: Int): Unit = {
    def readChar(): Option[Char] = try {
      Option(StdIn.readChar())
    } catch {
      case _: Throwable => None
    }

    def loop(points: List[LightRay], time: Int): Unit = {
      println()
      println(s"time: $time")
      println(view(update(points, time)))

      readChar() match {
        case Some(k) => k match {
          // Backward
          case 'b' => loop(points, time - 1)
          // Forward
          case 'f' => loop(points, time + 1)
          // Quit
          case 'q' | 'Q' =>

          case _ => loop(points, time)
        }

        case None => loop(points, time)
      }
    }

    loop(points, initTime)
  }

  // Test data
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

  // Answers
  // 10144
  // GGLZLHCE

  def main(args: Array[String]): Unit = {

    val file = new File("data/day10.txt")

    val points = parse(file)

    keyLoop(points, magicTime(points, 0))
  }
}



