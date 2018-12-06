package top

import java.io.File

import scala.io.Source
import scala.math._


object Day3 {

  type Fabric = Array[Array[Int]]

  case class Point(x: Int, y: Int)

  case class Rect(topLeft: Point, bottomRight: Point)

  val claimRegex = """#(\d{1,}) @ (\d{1,}),(\d{1,}): (\d{1,})x(\d{1,})""".r

  case class Claim(id: Int, x: Int, y: Int, width: Int, height: Int)

  object Claim {
    def apply(text: String): Claim = text match {
      case claimRegex(id, x, y, w, h) => Claim(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
      case _ => sys.exit(1)
    }
  }

  // Set the bits in the Fabric for any overlapping Claims
  private def update(fabric: Fabric, rect: Rect): Fabric = {
    val points = for {
      x <- rect.topLeft.x to rect.bottomRight.x
      y <- rect.topLeft.y to rect.bottomRight.y
    } yield Point(x, y)

    points.foreach(p => fabric(p.x)(p.y) = 1)
    fabric
  }

  // Calculate the Rect representing the overlap of two Claims, if any.
  private def overlap(l: Claim, r: Claim): Option[Rect] =
    if (isOverlapping(l, r))
      Option(Rect(Point(max(l.x, r.x), max(l.y, r.y)),
        Point(min(l.x + l.width - 1, r.x + r.width - 1), min(l.y + l.height - 1, r.y + r.height - 1))))
    else None


  // Determines if 2 Claims overlap
  private def isOverlapping(l: Claim, r: Claim): Boolean =
    l.x < r.x + r.width && l.y < r.y + r.height && r.x < l.x + l.width && r.y < l.y + l.height

  // For debugging
  def toString(fabric: Fabric): String =
    fabric.map(arr => arr.mkString(" ")).mkString("\n")


  def parse(lines: List[String]): List[Claim] =
    lines.map(Claim(_))

  // Parse the input file. Return a list of Claims.
  def parse(data: File): List[Claim] =
    parse(Source.fromFile(data).getLines().toList)

  // Find the largest x & y in the given Claims
  private def extent(claims: List[Claim]): Point = {
    val cx = claims.sortBy(c => c.x + c.width).reverse.head
    val cy = claims.sortBy(c => c.y + c.height).reverse.head
    Point(cx.x + cx.width, cy.y + cy.height)
  }


  def solve(claims: List[Claim]): (Int, List[Int]) = {
    def loop(_claims: List[Claim], fabric: Fabric, ids: Set[Int]): (Int, List[Int]) = _claims match {
      case Nil =>
        val f = fabric.foldLeft(0)((z, a) => z + a.sum)
        val is = claims.map(_.id).diff(ids.toSeq)
        (f, is)

      case head :: tail =>
        val (f, is) = tail.foldLeft((fabric, ids)) { case ((f, i), c) =>
          overlap(head, c).fold((f, i))(r => (update(f, r), i + head.id + c.id))
        }
        loop(tail, f, is)
    }

    // Get dimensions of Fabric
    val point = extent(claims)

    val fabric = Array.ofDim[Int](point.x, point.y)

    // Collect overlapping IDs
    loop(claims, fabric, Set.empty[Int])
  }

  def main(args: Array[String]): Unit = {

    val claims = parse(new File("data/day3.txt"))

    val answer = solve(claims)

    println(answer)
  }
}
