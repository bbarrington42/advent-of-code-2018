package top

object Day14 {


  def part1(target: Int): String = {
    def loop(recipes: IndexedSeq[Int], elf1: Int, elf2: Int): String = {
      if (recipes.length == target + 10)
        recipes.takeRight(10).mkString
      else {
        val sum = recipes(elf1) + recipes(elf2)

        val r = if (sum < 10) recipes :+ sum else recipes :+ sum / 10 :+ sum % 10

        val e1 = (elf1 + recipes(elf1) + 1) % r.size
        val e2 = (elf2 + recipes(elf2) + 1) % r.size
        loop(r, e1, e2)
      }
    }

    loop(IndexedSeq(3, 7), 0, 1)
  }

  def part2(target: String): Int = {
    def loop(recipes: IndexedSeq[Int], elf1: Int, elf2: Int): Int = {
      val x = recipes.takeRight(target.length + 1).mkString
      if (x.contains(target)) recipes.length - target.length else {
        val sum = recipes(elf1) + recipes(elf2)

        val r = if (sum < 10) recipes :+ sum else recipes :+ sum / 10 :+ sum % 10

        val e1 = (elf1 + recipes(elf1) + 1) % r.length
        val e2 = (elf2 + recipes(elf2) + 1) % r.length
        loop(r, e1, e2)
      }
    }

    loop(IndexedSeq(3, 7), 0, 1)
  }


  def main(args: Array[String]): Unit = {
    val r = part2("290431")

    println(r)
  }
}
