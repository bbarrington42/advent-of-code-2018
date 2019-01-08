package top

import java.io.File

import scala.annotation.tailrec
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

  def isCollision(cart: Cart, carts: List[Cart]): Boolean =
    carts.find(_.location == cart.location).isDefined

  def sort(carts: List[Cart]): List[Cart] =
    carts.sortBy(cart => (cart.location.y, cart.location.x))

  def getCarts(grid: Grid): List[Cart] = {

    def getCarts(y: Int, line: Array[Char]): List[Cart] =
      line.zipWithIndex.foldLeft(List.empty[Cart]) { case (z, (c, x)) => c match {
        case 'v' => z :+ Cart(Location(x, y), Down)
        case '^' => z :+ Cart(Location(x, y), Up)
        case '>' => z :+ Cart(Location(x, y), Right)
        case '<' => z :+ Cart(Location(x, y), Left)

        case _ => z
      }
      }

    sort(grid.zipWithIndex.foldLeft(List.empty[Cart]) { case (z, (l, y)) => z ++ getCarts(y, l) })
  }


  def advance(cart: Cart, grid: Grid): Cart = {

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

    val newLocation = cart.direction match {
      case Up => cart.location.copy(y = cart.location.y - 1)
      case Down => cart.location.copy(y = cart.location.y + 1)
      case Right => cart.location.copy(x = cart.location.x + 1)
      case Left => cart.location.copy(x = cart.location.x - 1)
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

  def part1(carts: List[Cart], grid: Grid): Location = {
    @tailrec
    def loop(current: List[Cart], previous: List[Cart]): Location = previous match {
      case Nil => loop(Nil, sort(current))

      case head :: tail =>
        val advanced = advance(head, grid)
        if (isCollision(advanced, tail)) advanced.location else
          loop(advanced :: current, tail)
    }

    loop(Nil, carts)
  }

  def removeAt[A](n: Int, list: List[A]): List[A] = {
    val (f, b) = list.splitAt(n)
    f ++ b.tail
  }

  def part2(carts: List[Cart], grid: Grid): Location = {
    @tailrec
    def loop(current: List[Cart], previous: List[Cart]): Location = previous match {
      case Nil => loop(Nil, sort(current))

      case head :: tail =>
        val advanced = advance(head, grid)
        if(current.isEmpty && tail.isEmpty) advanced.location else {
          val n1 = current.indexWhere(_.location == advanced.location)
          if(-1 != n1) loop(removeAt(n1, current), tail)
          else {
            val n2 = tail.indexWhere(_.location == advanced.location)
            if(-1 != n2) loop(current, removeAt(n2, tail))
            else loop(advanced :: current, tail)
          }
        }
    }

    loop(Nil, carts)
  }


  def main(args: Array[String]): Unit = {
    val file = new File("data/day13.txt")

    val grid = parse(file)

    val carts = getCarts(grid)

    val r = part2(carts, grid)

    println(r)

  }
}
