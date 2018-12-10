package top

import java.io.File
import java.lang.Math._

import scala.io.Source

/*
Calculate the bottom-right of the grid given the collection of points. Top-left is 0, 0.
For each point in the grid, calculate the Manhattan distance to each input point.
Increment the point count of the closest point EXCEPTING ties. Ties go to no one.
Eliminate the infinitely bounded points.
Return the highest count.
 */
object Day6 {

  case class Point(x: Int, y: Int)

  case class Rect(topLeft: Point, bottomRight: Point)

  def distance(p1: Point, p2: Point): Int =
    abs(p1.x - p2.x) + abs(p1.y - p2.y)

  def bottomRight(points: List[Point]): Point =
    Point(points.maxBy { case Point(x, _) => x }.x, points.maxBy { case Point(_, y) => y }.y)


  // Return the Point from the List that is closest to the given Point excepting ties. Return None if there is a tie.
  def closest(point: Point, points: List[Point]): Option[Point] = {
    val tuples = points.map(p => p -> distance(point, p)).sortBy { case (_, d) => d }
    //println(s"point: $point, tuples: $tuples")
    if (tuples.head._2 != tuples.tail.head._2) Some(tuples.head._1) else None
  }

  def grid(box: Rect): Seq[Point] =
    for {
      x <- box.topLeft.x to box.bottomRight.x
      y <- box.topLeft.y to box.bottomRight.y
    } yield Point(x, y)

  def counts(box: Rect, points: List[Point]): Map[Point, Int] = {

    val g = grid(box)

    g.foldLeft(Map.empty[Point, Int].withDefaultValue(0))((z, point) =>
      closest(point, points).fold(z)(p => z.updated(p, z(p) + 1)))
  }

  def solve2(points: List[Point]): Int = {
    val box = Rect(Point(0,0), bottomRight(points))

    val g = grid(box)

    // Walk the grid summing the distances to all points
    val d = g.map(p1 => points.map(p2 => distance(p1, p2)).sum)

    d.filter(_ < 10000).length
  }


  def solve1(points: List[Point]): Int = {

    val br = bottomRight(points)

    val box1 = Rect(Point(0, 0), br)

    val c1 = counts(box1, points)

    // Now increase the bounding box just slightly and remove those Points that change
    val box2 = Rect(Point(-1, -1), Point(br.x + 1, br.y + 1))

    val c2 = counts(box2, points)

    val finite = c1.filter { case (p, count) => c2(p) == count }

    finite.values.max
  }

  def parse(line: String): Point = {
    val arr = line.split(", ")
    Point(arr(0).toInt, arr(1).toInt)
  }

  def parse(lines: List[String]): List[Point] =
    lines.map(parse)

  def parse(file: File): List[Point] =
    parse(Source.fromFile(file).getLines().toList)

  val data = List(
    "1, 1",
    "1, 6",
    "8, 3",
    "3, 4",
    "5, 5",
    "7, 1",
    "8, 9")

  def main(args: Array[String]): Unit = {
    val file = new File("data/day6.txt")
    val points = parse(file)

    val r1 = solve1(points)

    println(s"part 1: $r1")

    val r2 = solve2(points)

    println(s"part2: $r2")
  }
}
