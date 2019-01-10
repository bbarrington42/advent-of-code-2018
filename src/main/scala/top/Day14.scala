package top

import java.util.Deque

import scala.collection.JavaConverters._

object Day14 {

  def lastN(n: Int, deque: Deque[Int]): Array[Int] = {
    def loop(i: Int, it: Iterator[Int], a: Array[Int]): Array[Int] =
      if (i == 0) a.reverse else if (it.hasNext) loop(i - 1, it, a :+ it.next()) else loop(i - 1, it, a)

    loop(n, deque.descendingIterator().asScala, Array.emptyIntArray)
  }

  def part1(target: Int): String = {
    def loop(recipes: Deque[Int], elf1: Int, elf2: Int): String = {
      if (recipes.size == target + 10)
        lastN(10, recipes).mkString
      else {
        val sum = recipes(elf1) + recipes(elf2)
        val r = if (sum < 10) recipes :+ sum else recipes :+ sum / 10 :+ sum % 10
        val e1 = (elf1 + recipes(elf1) + 1) % r.length
        val e2 = (elf2 + recipes(elf2) + 1) % r.length
        loop(r, e1, e2)
      }
    }

    loop(Array(3, 7), 0, 1)
  }


  def main(args: Array[String]): Unit = {
    val r = part1(290431)

    println(r)
  }
}
