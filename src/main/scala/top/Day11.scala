package top

object Day11 {

  type Grid = Array[Array[Int]]

  case class PowerSquare(x: Int, y: Int, dim: Int, power: Int)

  /*
  The power level in a given fuel cell can be found through the following process:

  Find the fuel cell's rack ID, which is its X coordinate plus 10.
  Begin with a power level of the rack ID times the Y coordinate.
  Increase the power level by the value of the grid serial number (your puzzle input).
  Set the power level to itself multiplied by the rack ID.
  Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
  Subtract 5 from the power level.
   */

  def power(x: Int, y: Int, serial: Int): Int = {
    val rack = x + 10
    val base = rack * y
    val p1 = base + serial
    val p2 = p1 * rack
    val p3 = (p2 / 100) % 10
    p3 - 5
  }


  def grid(serial: Int, height: Int = 300, width: Int = 300): Grid =
    Array.tabulate(height, width)((y, x) => power(x + 1, y + 1, serial))

  // Create the power square at the given position and dimension
  def powerCell(left: Int, top: Int, dim: Int, grid: Grid): PowerSquare = {
    //    val right = left + dim - 1
    //    val bottom = top + dim - 1
    //    assert(right <= grid(0).length & bottom <= grid.length)
    val cells = for {
      y <- top until top + dim
      x <- left until left + dim
    } yield (x, y)
    val power = cells.foldLeft(0) { case (z, (_x, _y)) => z + grid(_y - 1)(_x - 1) }
    PowerSquare(left, top, dim, power)
  }

  // Create all power squares with the given dimension and return the one with the greatest power in a Seq
  def powerCells(dim: Int, grid: Grid): Seq[PowerSquare] = {
    val cells = for {
      y <- 1 until grid.length - dim
      x <- 1 until grid(0).length - dim
    } yield powerCell(x, y, dim, grid)
    val sorted = cells.sortWith((l, r) => r.power - l.power < 0)
    sorted.headOption.map(c => Seq(c)).getOrElse(Seq.empty)
  }

  def huh(): Seq[Int] = for {
    x <- 1 until 0
  } yield x


  def main(args: Array[String]): Unit = {

    val g = grid(7139)

    val list = powerCells(3, g)

    println(list.head)

    println(huh)


    // Tests
    /////////////////////
    //
    //    assert(power(3, 5, 8) == 4)
    //    assert(power(122, 79, 57) == -5)
    //    assert(power(217, 196, 39) == 0)
    //    assert(power(101, 153, 71) == 4)
    //
    //    val g1 = grid(57)
    //    val g2 = grid(39)
    //    val g3 = grid(71)
    //    val g4 = grid(18)
    //    val g5 = grid(42)
    //
    //    Fuel cell at  122,79, grid serial number 57: power level -5.
    //    Fuel cell at 217,196, grid serial number 39: power level  0.
    //    Fuel cell at 101,153, grid serial number 71: power level  4.
    //    assert(g1(79 - 1)(122 - 1) == -5)
    //    assert(g2(196 - 1)(217 - 1) == 0)
    //    assert(g3(153 - 1)(101 - 1) == 4)
    //
    //
    //    assert(powerCell(33, 45, g4).power == 29)
    //    assert(powerCell(21, 61, g5).power == 30)

  }

}
