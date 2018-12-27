package top

import java.io.File

import scala.io.Source

object Day10 {
  // position=< 21373,  53216> velocity=<-2, -5>,
  val pointRegex =
    """position=<([^,]+),([^>]+)> velocity=<([^,]+),([^>]+)>""".r

  case class Point(x: Int, y: Int, vx: Int, vy: Int)

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
      if (p.x >= 0 && p.x < width && p.y >= 0 && p.y < height)
        z.updated(p.y, z(p.y).updated(p.x, '#')) else z
    })
    show(updated)
  }

  def show(display: Array[Array[Char]]): String =
    display.map(chars => chars.mkString).mkString("\n")

  def tick(points: List[Point], seconds: Long = 1): List[Point] =
    points.map { case Point(px, py, vx, vy) => Point(px + vx * seconds.toInt, py + vy * seconds.toInt, vx, vy) }

  def ticker(points: List[Point], display: Array[Array[Char]]): Unit = {
    val width = display(0).length
    val height = display.length

    def loop(time: Int, points: List[Point]): Unit = {
      // Check point coordinates
      val found = points.find(p => Math.abs(p.x) < 50 || Math.abs(p.y) < 50)

      // Calculate number of points within view 
      val contained = points.foldLeft(0)((z, p) =>
        if (p.x >= 0 && p.x < width && p.y >= 0 && p.y < height) z + 1 else z)
      println(s"$time: $contained")
      if (found.isEmpty)
        loop(time + 1, tick(points))
      else {
        val s = view(101, 101, points)
        println(s)
      }
    }

    loop(0, points)
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

    val width = 101
    val height = 101

    val testPoints = List(
      Point(0, 0, 0, 0),
      Point(width - 1, 0, 0, 0),
      Point(width - 1, height - 1, 0, 0),
      Point(0, height - 1, 0, 0))


    val ZERO_OFFSET = (width / 4, height / 4)
    val points = parse(file).map { case Point(x, y, vx, vy) => Point(x + ZERO_OFFSET._1, y + ZERO_OFFSET._2, vx, vy) }

    // Calculate x & y intercepts
    val intercepts = points.map { case Point(x, y, vx, vy) => (-x / vx, -y / vy) }

    // Find the least amount of time to intercept y-axis & x-axis
    val zipped = points.zip(intercepts)

    val tx = zipped.sortBy{case (p, z) => z._1}.head._2._1
    val ty = zipped.sortBy{case (p, z) => z._2}.head._2._2

    println(s"tx: $tx, ty: $ty")

    val blart = tick(points, ty)

    println(view(width, height, blart))


    //    val sorted = points.sortWith((l, r) => l.x + l.y > r.x + r.y)
    //
    //    println(sorted)

    //ticker(points, Array.fill(height, width)('.'))

    //    val display = view(width, height, tick(points, 3))
    //
    //    print(display)
  }
}



