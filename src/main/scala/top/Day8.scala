package top

import java.io.File

import scala.io.Source

object Day8 {

  case class Node(children: List[Node], metadata: List[Int])

  def parse(lines: List[String]): List[Int] = lines match {
    case Nil => Nil

    case head :: tail => head.split(" ").map(_.toInt).toList ++ parse(tail)
  }

  def parse(file: File): List[Int] = parse(Source.fromFile(file).getLines().toList)

  def node(data: List[Int]): (Node, List[Int]) = {

    val (ccount :: mcount :: tail) = data

    val (c, r1) = children(tail, Nil, ccount)
    val (m, r2) = metadata(r1, mcount)
    Node(c, m) -> r2
  }

  def metadata(data: List[Int], count: Int): (List[Int], List[Int]) = data.splitAt(count)


  def children(data: List[Int], nodes: List[Node], count: Int): (List[Node], List[Int]) = {
    // Must reverse as order is important for Part 2.
    if (count == 0) (nodes.reverse, data) else {
      val (n, d) = node(data)
      children(d, n :: nodes, count - 1)
    }
  }

  def solve1(input: (Node, List[Int])): Int = {
    def value(root: Node): Int = {
      def loop(children: List[Node]): Int = children match {
        case Nil => 0

        case head :: tail => tail.foldLeft(value(head))((z, n) => z + value(n))
      }

      root.metadata.sum + loop(root.children)
    }

    value(input._1)
  }


  /////////// Part 2 ////////////

  def solve2(input: (Node, List[Int])): Int = value(input._1)


  def value(node: Node): Int = value(node.children, node.metadata)


  def value(children: List[Node], refs: List[Int]): Int = children match {
    case Nil => refs.sum

    case _ => refs.foldLeft(0)((z, i) =>
      if (children.isDefinedAt(i - 1)) z + value(children(i - 1)) else z)
  }


  val input = List("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

  def main(args: Array[String]): Unit = {
    val file = new File("data/day8.txt")

    val data = //parse(input)
      parse(file)

    val root = node(data)

    val r1 = solve1(root)

    println(s"part 1: $r1")

    val r2 = solve2(root)

    println(s"part 2: $r2")
  }
}
