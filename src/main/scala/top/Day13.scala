package top

import java.io.File

import scala.io.Source

object Day13 {

  type Grid = Array[Array[Char]]


  sealed trait Turn

  case object RightTurn extends Turn

  case object LeftTurn extends Turn

  case object NoTurn extends Turn


  sealed trait Direction

  case object Left extends Direction

  case object Right extends Direction

  case object Up extends Direction

  case object Down extends Direction


  case class Location(x: Int, y: Int)

  case class Cart(location: Location, direction: Direction, lastTurn: Turn = RightTurn)

  implicit class WrappedGrid(grid: Grid) {
    def apply(location: Location): Char = grid(location.y)(location.x)
  }

  def isCollision(carts: List[Cart]): Option[Location] = carts match {
    case Nil => None

    case head :: tail =>
      tail.find(_.location == head.location).fold(isCollision(tail))(cart => Option(cart.location))
  }

  def getCarts(y: Int, line: Array[Char]): Array[Cart] = {
    line.zipWithIndex.foldLeft(Array.empty[Cart]) { case (z, (c, x)) => c match {
      case 'v' => z :+ Cart(Location(x, y), Down)
      case '^' => z :+ Cart(Location(x, y), Up)
      case '>' => z :+ Cart(Location(x, y), Right)
      case '<' => z :+ Cart(Location(x, y), Left)

      case _ => z
    }
    }
  }

  def getCarts(grid: Grid): List[Cart] =
    grid.zipWithIndex.foldLeft(List.empty[Cart]) { case (z, (l, y)) =>
      z ++ getCarts(y, l)
    }.sortBy(cart => (cart.location.y, cart.location.x))


  def advance(cart: Cart, grid: Grid): Cart = {
    val newLocation = cart.direction match {
      case Up => cart.location.copy(y = cart.location.y - 1)
      case Down => cart.location.copy(y = cart.location.y + 1)
      case Right => cart.location.copy(x = cart.location.x + 1)
      case Left => cart.location.copy(x = cart.location.x - 1)
    }

    def turn(cart: Cart): Cart = {
      val turn = cart.lastTurn match {
        case RightTurn => LeftTurn
        case LeftTurn => NoTurn
        case NoTurn => RightTurn
      }

      val updated = cart.copy(lastTurn = turn)

      turn match {
        case NoTurn => updated

        case LeftTurn =>
          cart.direction match {
            case Left => updated.copy(direction = Down)
            case Right => updated.copy(direction = Up)
            case Up => updated.copy(direction = Left)
            case Down => updated.copy(direction = Right)
          }

        case RightTurn =>
          cart.direction match {
            case Left => updated.copy(direction = Up)
            case Right => updated.copy(direction = Down)
            case Up => updated.copy(direction = Right)
            case Down => updated.copy(direction = Left)
          }
      }
    }

    grid(newLocation) match {
      case '|' | '^' | 'v' =>
        assert(cart.direction == Up || cart.direction == Down)
        cart.copy(location = newLocation)

      case '-' | '>' | '<' =>
        assert(cart.direction == Left || cart.direction == Right)
        cart.copy(location = newLocation)

      case '\\' => cart.direction match {
        case Up => cart.copy(location = newLocation, direction = Left)
        case Down => cart.copy(location = newLocation, direction = Right)
        case Right => cart.copy(location = newLocation, direction = Down)
        case Left => cart.copy(location = newLocation, direction = Up)
      }

      case '/' => cart.direction match {
        case Up => cart.copy(location = newLocation, direction = Right)
        case Down => cart.copy(location = newLocation, direction = Left)
        case Right => cart.copy(location = newLocation, direction = Up)
        case Left => cart.copy(location = newLocation, direction = Down)
      }

      case '+' => turn(cart.copy(location = newLocation))
    }
  }


  def parse(line: String): Array[Char] = line.toCharArray

  def parse(lines: List[String]): Grid =
    lines.foldLeft(Array.empty[Array[Char]])((z, line) => z :+ line.toCharArray)


  def parse(file: File): Grid = {
    val lines = Source.fromFile(file).getLines().toList
    parse(lines)
  }

  def main(args: Array[String]): Unit = {
    val file = new File("data/day13.txt")

    val grid = parse(file)

    val r = getCarts(grid)

    println(r.mkString)
  }
}
