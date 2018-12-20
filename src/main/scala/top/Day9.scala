package top

object Day9 {

  case class Circle(pos: Int = 0, marbles: IndexedSeq[Int])

  // Move current position CCW (-) or CW (+) by the specified amount. Return the updated buffer.
  def rotate(circle: Circle, delta: Int): Circle = {
    val len = circle.marbles.length
    val p = ((circle.pos + delta) % len + len) % len
    circle.copy(pos = p)
  }


  // Rotate to the right by one and insert the item between the current element and the item immediately to the right
  // of the current position. The current position points to the new item.
  // If there is no element immediately to the right of the current position, grow the items by one.
  def add(circle: Circle, item: Int): Circle = {
    val c = rotate(circle, 1)
    if (c.marbles.length - c.pos == 1)
      c.copy(pos = c.pos + 1, marbles = c.marbles :+ item) else {
      val (front, back) = c.marbles.splitAt(c.pos + 1)
      c.copy(pos = c.pos + 1, marbles = front ++ (item +: back))
    }
  }

  def remove(circle: Circle): (Int, Circle) = {
    val (front, back) = circle.marbles.splitAt(circle.pos)
    (back.head, circle.copy(marbles = front ++ back.tail))
  }


  def updateScores(player: Int, value: Int, scores: List[Int]): List[Int] = {
    val (front, back) = scores.splitAt(player)
    front ++ (back.head + value :: back.tail)
  }

  def nextPlayer(current: Int, scores: List[Int]): Int =
    (current + 1) % scores.length

  // One move by the designated player. Returns the updated scores and marble circle.
  def move(player: Int, marble: Int, scores: List[Int], circle: Circle): (List[Int], Circle) = {
    if (marble % 23 == 0) {
      val (v, c) = remove(rotate(circle, -7))
      // update this players score
      (updateScores(player, marble + v, scores), c)
    } else (scores, add(circle, marble))

  }


  def solve1(player: Int, marbles: List[Int], scores: List[Int], circle: Circle): Int = marbles match {
    case Nil => scores.max

    case marble :: rest =>
      val (s, c) = move(player, marble, scores, circle)
      solve1(nextPlayer(player, scores), rest, s, c)
  }

  def initMarbles(last: Int, marble: Int, marbles: List[Int]): List[Int] = {
    if (marble > last) marbles.reverse else
      initMarbles(last, marble + 1, marble :: marbles)
  }


  // 458 players; last marble is worth 71307 points
  /*
  10 players; last marble is worth 1618 points: high score is 8317
  13 players; last marble is worth 7999 points: high score is 146373
  17 players; last marble is worth 1104 points: high score is 2764
  21 players; last marble is worth 6111 points: high score is 54718
  30 players; last marble is worth 5807 points: high score is 37305
   */

  val NUM_PLAYERS = 458
  val LAST_MARBLE = 71307

  def main(args: Array[String]): Unit = {

    val scores = List.fill(NUM_PLAYERS)(0)

    val marbles = initMarbles(LAST_MARBLE, 1, Nil)

    val circle = Circle(0, IndexedSeq(0))

    val r1 = solve1(0, marbles, scores, circle)

    println(s"part 1: $r1")
  }
}
