package top

object Day14 {


  def part1(target: Int): String = {
    def loop(recipes: IndexedSeq[Int], elf1: Int, elf2: Int): String = {
      println(recipes.length)
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


  def main(args: Array[String]): Unit = {
    val r = part1(290431)

    println(r)
  }
}
