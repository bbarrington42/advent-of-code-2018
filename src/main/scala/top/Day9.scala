package top

import java.util

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


  def updateScores(player: Int, value: Int, scores: IndexedSeq[Int]): IndexedSeq[Int] =
    scores.updated(player, scores(player) + value)

  def nextPlayer(current: Int, scores: IndexedSeq[Int]): Int =
    (current + 1) % scores.length

  // One move by the designated player. Returns the updated scores and marble circle.
  def move(player: Int, marble: Int, scores: IndexedSeq[Int], circle: Circle): (IndexedSeq[Int], Circle) = {
    if (marble % 23 == 0) {
      val (v, c) = remove(rotate(circle, -7))
      // update this players score
      (updateScores(player, marble + v, scores), c)
    } else (scores, add(circle, marble))

  }


  def solve1(player: Int, marbles: IndexedSeq[Int], scores: IndexedSeq[Int], circle: Circle): Int = {
    if (marbles.isEmpty) scores.max else {
      val (s, c) = move(player, marbles.head, scores, circle)
      solve1(nextPlayer(player, scores), marbles.tail, s, c)
    }
  }

  def initMarbles(last: Int, marble: Int, marbles: Array[Int]): Array[Int] = {
    if (marble > last) marbles else
      initMarbles(last, marble + 1, marbles :+ marble)
  }


  ///// Part 2 ////
  // Use a Deque since technique in part 1 is too slow
  // The current marble will always be at the head of the deque

  def solve2(player: Int, marble: Int, scores: Array[Int], circle: util.ArrayDeque[Int]): Int = {
    if (marble > LAST_MARBLE) scores.max else {
      val (s, c) = move2(player, marble, scores, circle)
      solve2((player + 1) % scores.length, marble + 1, s, c)
    }
  }

  def move2(player: Int, marble: Int, scores: Array[Int], circle: util.ArrayDeque[Int]): (Array[Int], util.ArrayDeque[Int]) = {
    if (marble % 10000 == 0) println(marble)
    if (marble % 23 == 0) {
      val c = rotate2(circle, -7)
      val v = c.removeFirst()
      (scores.updated(player, scores(player) + marble + v), c)
    } else {
      //println(s"before: $circle")
      val c = rotate2(circle, 2)
      //println(s"after: $c")
      c.addFirst(marble)
      (scores, c)
    }
  }

  def rotate2(marbles: util.ArrayDeque[Int], by: Int): util.ArrayDeque[Int] = {
    def loop(count: Int): util.ArrayDeque[Int] = {
      if (count == 0) marbles else {
        val first = marbles.removeFirst()
        marbles.addLast(first)
        loop(count - 1)
      }
    }

    val len = marbles.size()
    val pos = ((by % len) + len) % len
    loop(pos)
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
  val LAST_MARBLE = 7130700

  def main(args: Array[String]): Unit = {

    val scores = Array.fill(NUM_PLAYERS)(0)
    //val marbles = initMarbles(LAST_MARBLE, 1, Array.empty)
    val circle = new util.ArrayDeque[Int]()
    circle.addFirst(0)
    val r2 = solve2(0, 1, scores, circle)

    println(s"answer: $r2")

    //
    //    val scores = IndexedSeq.fill(NUM_PLAYERS)(0)
    //
    //    val marbles = initMarbles(LAST_MARBLE, 1, IndexedSeq.empty)
    //
    //    val circle = Circle(0, IndexedSeq(0))
    //
    //    val r1 = solve1(0, marbles, scores, circle)
    //
    //    println(s"part 1: $r1")
  }
}
