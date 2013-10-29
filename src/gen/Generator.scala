package gen

case class Point(y: Int, x: Int) {
  def addX(d: Int) = Point(y, x+d)
  def addY(d: Int) = Point(y+d, x)
}
case class Square(topLeft: Point, size: Int) {
  def topRight = topLeft.addX(size)
  def bottomLeft = topLeft.addY(size)
  def bottomRight = bottomLeft.addX(size)
}

case class Grid(size: Int) {
  val points: List[Point] =
    ( for (y <- 1 to size; x <- 1 to size) yield Point(y, x) ).toList
  val squares: List[Square] =
    ( for (sqSize <- 1 to (size - 1); point <- Grid(size-sqSize).points) yield Square(point, sqSize) ).toList
}

object Generator {

  type States = List[Boolean]

  case class Trial(states: States) {
  }

  val grid = Grid(4)
  val numOfStates = grid.size * grid.size

  def randomBoolean: Boolean = Math.random() > 0.5

  def createStates: States = ( for(i <- 1 to numOfStates) yield randomBoolean ).toList

  def validStates(states: States): Boolean = { val s = states.map(if (_) 1 else 0).sum; s >= 6 && s <= 10}

}

