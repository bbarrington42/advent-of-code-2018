package top

import java.io.File

import scala.io.{Source, StdIn}

object Day10 {
  // position=< 21373,  53216> velocity=<-2, -5>,
  val pointRegex =
    """position=<([^,]+),([^>]+)> velocity=<([^,]+),([^>]+)>""".r

  case class Point(x: Int, y: Int, vx: Int, vy: Int)

  case class Intercept(time: Long, coordinate: Int)

  def xform(line: String): Point = line match {
    case pointRegex(px, py, vx, vy) => Point(px.trim.toInt, py.trim.toInt, vx.trim.toInt, vy.trim.toInt)
  }

  def parse(file: File): List[Point] = {
    val lines = Source.fromFile(file).getLines().toList
    parse(lines)
  }

  def parse(lines: List[String]): List[Point] =
    lines.map(xform)

  // Calculate the x & y intercepts for a Point. (x, y)
  def intercept(point: Point): (Intercept, Intercept) = {
    // Times
    val xtime = -point.y / point.vy
    val ytime = -point.x / point.vx
    val xint = point.x + point.vx * xtime
    val yint = point.y + point.vy * ytime
    (Intercept(xtime, xint), Intercept(ytime, yint))
  }

  def intercepts(points: List[Point]): List[(Intercept, Intercept)] =
    points.map(intercept)


  // return ((top, left), (bottom, right))
  def boundingBox(intercepts: List[(Intercept, Intercept)]): ((Intercept, Intercept), (Intercept, Intercept)) = {
    // Separate the intercepts by lowest time
    val (xint, yint) = intercepts.foldLeft((List.empty[Intercept], List.empty[Intercept])) { case ((xi, yi), (x, y)) =>
      if (x.time < y.time) (x :: xi, yi) else (xi, y :: yi)
    }

    val top = yint.minBy(_.coordinate)
    val bottom = yint.maxBy(_.coordinate)
    val left = xint.minBy(_.coordinate)
    val right = xint.maxBy(_.coordinate)

    ((top, left), (bottom, right))
  }

  // For a given set of points, create a view to show them
  def view(width: Int, height: Int, points: List[Point]): String = {
    // Create an array first
    val array = Array.fill[Char](height, width)('.')
    // Translate the points to the center of the display
    val OFFSET = (width / 2, height / 2)
    // Update the visible points
    val updated = points.map { case Point(x, y, vx, vy) => Point(x + OFFSET._1, y + OFFSET._2, vx, vy) }.foldLeft(array)((z, p) => {
      if (p.x >= 0 && p.x < width && p.y >= 0 && p.y < height)
        z.updated(p.y, z(p.y).updated(p.x, '#')) else z
    })
    updated.map(chars => chars.mkString).mkString("\n")
  }

  def update(points: List[Point], seconds: Long): List[Point] =
    points.map { case Point(px, py, vx, vy) => Point(px + vx * seconds.toInt, py + vy * seconds.toInt, vx, vy) }


  // Enter loop to control display with keyboard
  def keyLoop(width: Int, height: Int, points: List[Point]): Unit = {
    def readChar(): Option[Char] = try {
      Option(StdIn.readChar())
    } catch {
      case _: Throwable => None
    }

    def loop(points: List[Point]): Unit = {
      println(s"----------------------------------------------------------------------------------")
      println(view(width, height, points))

      readChar() match {
        case Some(k) => k match {
          case 'z' => loop(update(points, -1))
          case 'x' => loop(update(points, 1))
          case 'q' | 'Q' =>

          case _ => loop(points)
        }

        case None => loop(points)
      }
    }

    loop(points)
  }

  def timeLoop(width: Int, height: Int, points: List[Point], initTime: Long): Unit = {


    def loop(time: Long): Unit = {
      println(s"time: $time")
      println(view(width, height, update(points, time)))

      Thread.sleep(1000)

      loop(time + 1)

    }

    loop(initTime)
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

    val points = parse(file)

    val ((top, left), (bottom, right)) = boundingBox(intercepts(points))

    val width = right.coordinate - left.coordinate
    val height = bottom.coordinate - top.coordinate

    val time = List(top, left, bottom, right).minBy(_.time).time

    timeLoop(width / 16, height / 32, update(points, time), time)

  }
}



