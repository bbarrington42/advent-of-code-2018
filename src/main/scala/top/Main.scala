package top

import java.io.File

import top.Day3._

object Main {

  // Some test data
  val data = List(
    "#1 @ 1,3: 4x4",
    "#2 @ 3,1: 4x4",
    "#3 @ 5,5: 2x2"
  )

  def main(args: Array[String]): Unit = {

    val claims = parse(new File("data/input.txt"))

    val answer = solve(claims)

    println(answer)
  }

}
