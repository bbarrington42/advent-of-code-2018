package top

object Day9 {

  case class CircularBuffer[A](pos: Int = 0, items: List[A]) {

    // Move current position CCW (-) or CW (+) by the specified amount. Return the updated buffer.
    def rotate(delta: Int): CircularBuffer[A] = {
      println(s"rotate: this: $this, delta: $delta")
      val len = items.length
      val p = ((pos + delta) % len + len) % len
      println(s"p: $p")
      copy(pos = p)
    }


    // Rotate to the right by one and insert the item between the current element and the item immediately to the right
    // of the current position. The current position points to the new item.
    // If there is no element immediately to the right of the current position, grow the items by one.
    def add(item: A): CircularBuffer[A] = {
      val buf = rotate(1)
      println(s"buf: $buf")
      if (buf.items.length - buf.pos == 1) buf.copy(pos = buf.pos + 1, items = (item :: buf.items.reverse).reverse) else {
        val (front, back) = buf.items.splitAt(buf.pos + 1)
        println(s"splitAt ${buf.pos + 1} ==> front: $front, back: $back")
        buf.copy(pos = buf.pos + 1, items = front ++ (item :: back))
      }
    }

    def remove(): CircularBuffer[A] = {
      val (front, back) = items.splitAt(pos)
      copy(items = front ++ back.tail)
    }

  }


  def play(marble: Int, circle: CircularBuffer[Int]): CircularBuffer[Int] =
    circle.add(marble)


  def loop(count: Int, marble: Int, circle: CircularBuffer[Int]): List[CircularBuffer[Int]] =
    if (count == 0) Nil else
      circle :: loop(count - 1, marble + 1, play(marble, circle))


  // 458 players; last marble is worth 71307 points
  def main(args: Array[String]): Unit = {

    val init = CircularBuffer[Int](0, List(0))

    val r = loop(20, 1, init)

    println(r.length)
    println(r)

  }
}
