package top

import java.util

object Day9 {


  // Use a Deque.
  // The current marble will always be at the head of the deque

  def solve(player: Int, marble: Int, scores: Array[Long], circle: util.ArrayDeque[Int]): Long = {
    if (marble > LAST_MARBLE) scores.max else {
      val (s, c) = move(player, marble, scores, circle)
      solve((player + 1) % scores.length, marble + 1, s, c)
    }
  }

  def move(player: Int, marble: Int, scores: Array[Long], circle: util.ArrayDeque[Int]): (Array[Long], util.ArrayDeque[Int]) = {
    if (marble % 23 == 0) {
      val c = rotate(circle, -7)
      val v = c.removeFirst()
      (scores.updated(player, scores(player) + marble + v), c)
    } else {
      val c = rotate(circle, 2)
      c.addFirst(marble)
      (scores, c)
    }
  }

  def rotate(marbles: util.ArrayDeque[Int], by: Int): util.ArrayDeque[Int] = {
    val left: util.ArrayDeque[Int] => Unit = deque => {
      val last = deque.removeLast()
      deque.addFirst(last)
    }

    val right: util.ArrayDeque[Int] => Unit = deque => {
      val first = deque.removeFirst()
      deque.addLast(first)
    }

    def loop(count: Int, f: util.ArrayDeque[Int] => Unit): util.ArrayDeque[Int] = {
      if (count == 0) marbles else {
        f(marbles)
        loop(count - 1, f)
      }
    }

    loop(Math.abs(by), if (by < 0) left else right)
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

    val scores = Array.fill(NUM_PLAYERS)(0L)
    val circle = new util.ArrayDeque[Int]()
    circle.addFirst(0)
    val r = solve(0, 1, scores, circle)

    println(s"answer: $r")
  }
}
