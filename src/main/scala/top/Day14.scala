package top

import java.util.{ArrayDeque, Deque}

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
        val x = recipes.toArray()(elf1)
        val y = recipes.toArray()(elf2)

        val sum = x.asInstanceOf[Int] + y.asInstanceOf[Int]
        println(s"size: ${recipes.size}")
        val r = if (sum < 10) {
          recipes.addLast(sum)
          recipes
        } else {
          recipes.addLast(sum / 10)
          recipes.addLast(sum % 10)
          recipes
        }
        val a = recipes.toArray()
        val e1 = (elf1 + a(elf1).asInstanceOf[Int] + 1) % r.size
        val e2 = (elf2 + a(elf2).asInstanceOf[Int] + 1) % r.size
        loop(r, e1, e2)
      }
    }

    val d = new ArrayDeque[Int]()
    d.addFirst(7)
    d.addFirst(3)
    loop(d, 0, 1)
  }


  def main(args: Array[String]): Unit = {
    val r = part1(290431)

    println(r)
  }
}
