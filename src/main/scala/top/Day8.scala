package top

import java.io.File

import scala.io.Source

object Day8 {

  case class Node(children: List[Node], metadata: List[Int])

  def parse(lines: List[String]): List[Int] = lines match {
    case Nil => Nil

    case head :: tail => head.split(" ").map(_.toInt).toList ++ parse(tail)
  }

  def parse(file: File): List[Int] =
    parse(Source.fromFile(file).getLines().toList)

  def node(data: List[Int]): (Node, List[Int]) = {

    val ccount = data.head
    val mcount = data.tail.head

    val (c, r1) = children(data.drop(2), Nil, ccount)
    val (m, r2) = metadata(r1, mcount)
    (Node(c, m), r2)
  }

  def metadata(data: List[Int], count: Int): (List[Int], List[Int]) =
    data.splitAt(count)


  def children(data: List[Int], nodes: List[Node], count: Int): (List[Node], List[Int]) = {
    if (count == 0) (nodes, data) else {
      val (n, d) = node(data)
      children(d, n :: nodes, count - 1)
    }
  }

  def solve1(root: Node): Int = {
    
  }


  def main(args: Array[String]): Unit = {
    val file = new File("data/day8.txt")

    val data = parse(file)

    val root = node(data)

    println(root)
  }
}
