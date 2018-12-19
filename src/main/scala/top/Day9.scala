package top

object Day9 {

  case class MarbleCircle(pos: Int = 0, marbles: List[Int]) {

    // Move current position CCW (-) or CW (+) by the specified amount. Return the updated buffer.
    def rotate(delta: Int): MarbleCircle = {
      //println(s"rotate: this: $this, delta: $delta")
      val len = marbles.length
      val p = ((pos + delta) % len + len) % len
      //println(s"p: $p")
      copy(pos = p)
    }


    // Rotate to the right by one and insert the item between the current element and the item immediately to the right
    // of the current position. The current position points to the new item.
    // If there is no element immediately to the right of the current position, grow the items by one.
    def add(item: Int): MarbleCircle = {
      val buf = rotate(1)
      //println(s"buf: $buf")
      if (buf.marbles.length - buf.pos == 1)
        buf.copy(pos = buf.pos + 1, marbles = (item :: buf.marbles.reverse).reverse) else {
        val (front, back) = buf.marbles.splitAt(buf.pos + 1)
        //println(s"splitAt ${buf.pos + 1} ==> front: $front, back: $back")
        buf.copy(pos = buf.pos + 1, marbles = front ++ (item :: back))
      }
    }

    def remove(): MarbleCircle = {
      val (front, back) = marbles.splitAt(pos)
      copy(marbles = front ++ back.tail)
    }

  }


  def play(marble: Int, circle: MarbleCircle): MarbleCircle =
    circle.add(marble)


  def loop(count: Int, marble: Int, circle: MarbleCircle): List[MarbleCircle] =
    if (count == 0) Nil else
      circle :: loop(count - 1, marble + 1, play(marble, circle))


  // 458 players; last marble is worth 71307 points
  def main(args: Array[String]): Unit = {

    val init = MarbleCircle(0, List(0))

    val r = loop(20, 1, init)

    println(r.length)
    println(r)

  }
}
