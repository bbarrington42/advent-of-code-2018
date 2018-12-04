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
    if (isOverlap(l, r))
      Option(Rect(Point(max(l.x, r.x), max(l.y, r.y)),
        Point(min(l.x + l.width - 1, r.x + r.width - 1), min(l.y + l.height - 1, r.y + r.height - 1))))
    else None


  // Determines if 2 Claims overlap
  private def isOverlap(l: Claim, r: Claim): Boolean =
    l.x < r.x + r.width && l.y < r.y + r.height && r.x < l.x + l.width && r.y < l.y + l.height


  def toString(fabric: Fabric): String =
    fabric.map(arr => arr.mkString(" ")).mkString("\n")


  def parse(lines: List[String]): List[Claim] =
    lines.map(Claim(_))

  // Parse the input file. Return an ordered list of Claims
  def parse(data: File): List[Claim] = {
    val lines = Source.fromFile(data).getLines().toList
    parse(lines)
  }

  // Find the largest x & y in the given Claims
  def extent(claims: List[Claim]): Point = {
    val cx = claims.sortBy(c => c.x + c.width).reverse.head
    val cy = claims.sortBy(c => c.y + c.height).reverse.head
    Point(cx.x + cx.width, cy.y + cy.height)
  }

  def solve(claims: List[Claim], fabric: Fabric): Int = claims match {
    case Nil => fabric.foldLeft(0)((z, a) => z + a.sum)

    case head :: tail =>
      val f = tail.foldLeft(fabric)((f, c) => overlap(head, c).fold(f)(r => update(f, r)))
      solve(tail, f)
  }

}
