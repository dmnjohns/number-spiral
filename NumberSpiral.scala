object NumberSpiral extends App {

  object Direction extends Enumeration {
    type Direction = Value
    val Right, Left, Up, Down = Value
  }

  import Direction._

  case class Coordinate(x: Int, y: Int)

  //todo: take a number as input
  //todo: parameterize streams to accept a seed value
  //note: using def rather than val to prevent memoization
  var curX = 0
  var curY = 0
  val sideOfSquare = 5
  val numberIterator: Iterator[Int] = {
    def numberStream(seed: Int): Stream[Int] = seed #:: numberStream(seed + 1)
    numberStream(1).iterator
  }
  val directionIterator: Iterator[Direction] = {
    def directionStream: Stream[Direction] = Down #:: Left #:: Up #:: Right #:: directionStream
    directionStream.iterator
  }

  def spiralStream(n: Int): Stream[Int] =
    if (n == 0) Stream.empty
    else n #:: (n - 1) #:: spiralStream(n - 1)

  val spiralMatrix = Array.ofDim[Int](sideOfSquare, sideOfSquare)
  var curDirection = Right
  spiralStream(sideOfSquare) foreach { lengthOfSide =>
    1 to lengthOfSide foreach { i =>
      spiralMatrix(curY)(curX) = numberIterator.next()
      if (i == lengthOfSide) {
        curDirection = directionIterator.next()
      }
      curDirection match {
        case Right => curX += 1
        case Left => curX -= 1
        case Up => curY -= 1
        case Down => curY += 1
      }
    }
  }

  println(spiralMatrix.map(_.mkString("\t")).mkString("\n"))
}
