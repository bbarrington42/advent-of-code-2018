package top

import java.io.File

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

  case class Cart(location: Location, direction: Direction, lastTurn: Turn)

  implicit class WrappedGrid(grid: Grid) {
    def apply(location: Location): Char = grid(location.y)(location.x)
  }


  def advance(cart: Cart, grid: Grid): Cart = {
    val newLocation = cart.direction match {
      case Up => cart.location.copy(y = cart.location.y - 1)
      case Down => cart.location.copy(y = cart.location.y + 1)
      case Right => cart.location.copy(x = cart.location.x + 1)
      case Left => cart.location.copy(x = cart.location.x - 1)
    }

    def turn(location: Location, cart: Cart): Cart = ()

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
        case Down =>cart.copy(location = newLocation, direction = Left)
        case Right =>cart.copy(location = newLocation, direction = Up)
        case Left => cart.copy(location = newLocation, direction = Down)
      }

      case '+' => cart
    }
  }


  def update(cart: Cart, grid: Grid): Cart = {
    val advanced = advance(cart)
  }

  def parse(line: String): Array[Char] = line.toCharArray

  def parse(lines: List[String]): Grid = {
    lines.foldLeft(Array.emptyCharArray)((z, ))
  }

  def parse(file: File): Grid = ???
}
