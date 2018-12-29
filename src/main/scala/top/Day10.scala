package top

import java.io.File

import scala.io.{Source, StdIn}
import scala.util.Random

object Day10 {
  // position=< 21373,  53216> velocity=<-2, -5>,
  val pointRegex =
    """position=<([^,]+),([^>]+)> velocity=<([^,]+),([^>]+)>""".r

  case class Point(x: Int, y: Int, vx: Int, vy: Int)

  def xform(line: String): Point = line match {
    case pointRegex(px, py, vx, vy) => Point(px.trim.toInt, py.trim.toInt, vx.trim.toInt, vy.trim.toInt)
  }

  def parse(file: File): List[Point] = {
    val lines = Source.fromFile(file).getLines().toList
    parse(lines)
  }

  def parse(lines: List[String]): List[Point] =
    lines.map(xform)

  // Calculate the x & y intercepts for a Point.
  // Values are returned in a tuple of tuples, i.e. ((time, x intercept), (time, y intercept))
  def intercept(point: Point): ((Long, Int), (Long, Int)) = {
    // Times
    val xit = -point.y / point.vy
    val yit = -point.x / point.vx
    val xint = point.x + point.vx * xit
    val yint = point.y + point.vy * yit
    ((xit, xint), (yit, yint))
  }

  def intercepts(points: List[Point]): List[((Long, Int), (Long, Int))] =
    points.map(intercept)

  // return ((top, left), (bottom, right))
  def boundingBox(intercepts: List[((Long, Int), (Long, Int))]): ((Int, Int), (Int, Int)) = {
    val top = intercepts.minBy { case ((tx, x), (ty, y)) => y }._2._2
    val left = intercepts.minBy { case ((tx, x), (ty, y)) => x }._1._2
    val bottom = intercepts.maxBy { case ((tx, x), (ty, y)) => y }._2._2
    val right = intercepts.maxBy { case ((tx, x), (ty, y)) => x }._1._2
    ((top, left), (bottom, right))
  }

  // For a given set of points, create a view to show them
  def view(width: Int, height: Int, points: List[Point]): String = {
    // Create an array first
    val array = Array.fill[Char](height, width)('.')
    // Translate the points to the center of the display
    val OFFSET = (width / 4, height / 4)
    // Update the visible points
    val updated = points.map { case Point(x, y, vx, vy) => Point(x + OFFSET._1, y + OFFSET._2, vx, vy) }.foldLeft(array)((z, p) => {
      if (p.x >= 0 && p.x < width && p.y >= 0 && p.y < height)
        z.updated(p.y, z(p.y).updated(p.x, '#')) else z
    })
    updated.map(chars => chars.mkString).mkString("\n")
  }

  def tick(points: List[Point], seconds: Long = 1): List[Point] =
    points.map { case Point(px, py, vx, vy) => Point(px + vx * seconds.toInt, py + vy * seconds.toInt, vx, vy) }

  // Pick one point at random. Increment time and sum the distances to all other points.
  // Continue until the value starts to increase. Use Manhattan distance
  def magicTime(points: List[Point]): Int = {
    def loop(time: Int, point: Point, points: List[Point], sum: Int): Int = {
      val newSum = points.foldLeft(0)((z, p) => z + Math.abs(point.x - p.x) + Math.abs(point.y - p.y))
      if (newSum > sum) time else
        loop(time + 1, point, tick(points), newSum)
    }

    val startPoint = points(Random.nextInt(points.length))
    val startSum = points.foldLeft(0)((z, p) => z + Math.abs(startPoint.x - p.x) + Math.abs(startPoint.y - p.y))
    loop(0, startPoint, points, startSum)
  }


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
          case 'z' => loop(tick(points, -1))
          case 'x' => loop(tick(points, 1))
          case 'q' | 'Q' =>

          case _ => loop(points)
        }

        case None => loop(points)
      }
    }

    loop(points)
  }

  def timeLoop(initTime: Int, width: Int, height: Int, points: List[Point]): Unit = {

    def getTime(): Int = StdIn.readInt()


    def loop(time: Int): Unit = {
      println(s"time: $time")
      println(view(width, height, points.map { case Point(x, y, vx, vy) => Point(x + vx * time, y + vy * time, vx, vy) }))

      loop(getTime())

    }

    loop(initTime)
  }

  // Calculate the time at which each point comes closest to the origin
  def timeCalc(p: Point): Long =
    (-p.x * p.vx - p.y * p.vy) / (p.vx * p.vx + p.vy * p.vy)


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

    val width = 81
    val height = 25

    val points = parse(file)

    println(boundingBox(intercepts(points)))

    //    val times = points.map(timeCalc).sorted
    //
    //    println(times.mkString("\n"))

    //timeLoop(10300, width, height, points)

  }
}



