package top

import java.io.File

import scala.io.Source

object Day8 {

  case class Node(name: Int, children: List[Node], metadata: List[Int]) {
    override def toString: String = s"name: ${name.toChar}, children: $children, metadata: $metadata"
  }

  def parse(lines: List[String]): List[Int] = lines match {
    case Nil => Nil

    case head :: tail => head.split(" ").map(_.toInt).toList ++ parse(tail)
  }

  def parse(file: File): List[Int] =
    parse(Source.fromFile(file).getLines().toList)

  def node(name: Int, data: List[Int]): (Node, List[Int]) = {

    val ccount = data.head
    val mcount = data.tail.head

    val (c, r1) = children(name + 1, data.drop(2), Nil, ccount)
    val (m, r2) = metadata(r1, mcount)
    (Node(name, c, m), r2)
  }

  def metadata(data: List[Int], count: Int): (List[Int], List[Int]) =
    data.splitAt(count)


  def children(name: Int, data: List[Int], nodes: List[Node], count: Int): (List[Node], List[Int]) = {
    if (count == 0) (nodes.reverse, data) else {
      val (n, d) = node(name, data)
      children(name + 1, d, n :: nodes, count - 1)
    }
  }

  def solve1(root: Node): Int = {
    def loop(children: List[Node]): Int = children match {
      case Nil => 0

      case head :: tail => tail.foldLeft(solve1(head))((z, n) => z + solve1(n))
    }

    root.metadata.sum + loop(root.children)
  }


  /////////// Part 2 ////////////

  def solve2(node: Node): Int = value(node.children, node.metadata)


  def value(children: List[Node], indices: List[Int]): Int = children match {
    case Nil => indices.sum

    case _ => indices.foldLeft(0)((z, i) =>
      if (children.isDefinedAt(i - 1)) z + solve2(children(i - 1)) else z)

  }


  val input = List("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

  def main(args: Array[String]): Unit = {
    val file = new File("data/day8.txt")

    val data = //parse(input)
    parse(file)

    val root = node('A', data)

    println(solve1(root._1))

    println(solve2(root._1))
  }
}
