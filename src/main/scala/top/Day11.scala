package top

object Day11 {

  /*
  The power level in a given fuel cell can be found through the following process:

Find the fuel cell's rack ID, which is its X coordinate plus 10.
Begin with a power level of the rack ID times the Y coordinate.
Increase the power level by the value of the grid serial number (your puzzle input).
Set the power level to itself multiplied by the rack ID.
Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
Subtract 5 from the power level.

Fuel cell at  122,79, grid serial number 57: power level -5.
Fuel cell at 217,196, grid serial number 39: power level  0.
Fuel cell at 101,153, grid serial number 71: power level  4.

   */


  def power(x: Int, y: Int, serial: Int): Int = {
    val rack = x + 10
    val base = rack * y
    val p1 = base + serial
    val p2 = p1 * rack
    val p3 = (p2 / 100) % 10
    p3 - 5
  }


  def grid(serial: Int, height: Int = 300, width: Int = 300): Array[Array[Int]] =
    Array.tabulate(height, width)((y, x) => power(x + 1, y + 1, serial))


  def main(args: Array[String]): Unit = {


    // Tests
    assert(power(3, 5, 8) == 4)
    assert(power(122, 79, 57) == -5)
    assert(power(217, 196, 39) == 0)
    assert(power(101, 153, 71) == 4)

    val g = grid(57)

    assert(g(79-1)(122-1) == -5)

    val l = g.map(_.toList).toList

    println(l.map(_.mkString(", ")).mkString("\n"))
  }

}
