package gen

import scala.collection.mutable

case class Point(y: Int, x: Int) {
  def addX(d: Int) = Point(y, x+d)
  def addY(d: Int) = Point(y+d, x)
}

case class Square(topLeft: Point, size: Int) {
  val topRight = topLeft.addX(size)
  val bottomLeft = topLeft.addY(size)
  val bottomRight = bottomLeft.addX(size)
  val points: List[Point] = List(topLeft, topRight, bottomLeft, bottomRight)
}

case class Grid(size: Int) {
  val points: List[Point] =
    ( for (y <- 1 to size; x <- 1 to size) yield Point(y, x) ).toList
  val squares: List[Square] =
    ( for (sqSize <- 1 to (size - 1); point <- Grid(size-sqSize).points) yield Square(point, sqSize) ).toList
}

trait RandomLists {

  val numOfStates: Int

  import org.scalacheck._
  import Gen._

  private lazy val randomBooleanGen = oneOf(true, false)
  private lazy val booleanListsGen = containerOfN[List,Boolean](numOfStates,randomBooleanGen)
  def createBooleanList = booleanListsGen.sample.get
  def randomBoolean = randomBooleanGen.sample.get
}

trait Parameters {

  val minMarkedNodes = 6
  val maxMarkedNodes =  10

  val numOfVisualTargets = 25
  val numOfAudioTargets = 25
  val numOfNonTargets = 25
  val numOfTrials = numOfVisualTargets + numOfAudioTargets + numOfNonTargets

}

object Generator extends RandomLists with Parameters {

  type States = List[Boolean]

  val GRID = Grid(4)
  val numOfStates = GRID.size * GRID.size

  def index(y: Int, x: Int): Int = (y - 1) * GRID.size + (x - 1)

  case class MarkedGrid(states: States)  {
    require(states.size == numOfStates)
    def markedPoint(p: Point): Boolean = states(index(p.y, p.x))
    def markedSquare(s: Square): Boolean = s.points.forall(markedPoint)
    val markedSquares: List[Square] = GRID.squares.filter(markedSquare)
    val visualTarget: Boolean = markedSquares.size > 0
  }

  case class Trial(grid: MarkedGrid, audioTarget: Boolean) {
    val visualTarget: Boolean = grid.visualTarget
    val valid: Boolean = ! visualTarget && audioTarget
    val target: Boolean = visualTarget || audioTarget
  }

  class Experiment {
    private val trials = new mutable.MutableList[Trial]()
    private val statesList = new mutable.MutableList[States]()
    private def visuals: List[Trial] = trials.filter(_.grid.visualTarget).toList
    private def audios: List[Trial] = trials.filter(_.audioTarget).toList
    private def nonTargets: List[Trial] = trials.filter(! _.target).toList
    private def uniqueStates(t: Trial): Boolean = ! statesList.contains(t.grid.states)
    private def acceptableVisual(t: Trial): Boolean = t.visualTarget && visuals.size < numOfVisualTargets
    private def acceptableAudio(t: Trial): Boolean = t.audioTarget && audios.size < numOfAudioTargets
    private def acceptableNonTarget(t: Trial): Boolean = (!t.target) && nonTargets.size < numOfNonTargets
    private def acceptable(t: Trial): Boolean = uniqueStates(t) && (acceptableVisual(t) || acceptableAudio(t) || acceptableNonTarget(t))
    def add(trial: Trial): Unit = if (acceptable(trial)) {
      trials += trial
      statesList += trial.grid.states
    }
    def complete: Boolean = trials.size >= numOfTrials
    def getTrials: List[Trial] = trials.toList
  }

  def validStates(states: States): Boolean = {
    val nm: Int = states.count(b => b)
    nm >= minMarkedNodes && nm <= maxMarkedNodes
  }

  def createValidStates: States = {val states = createBooleanList ; if (validStates(states)) states else createValidStates}

  def createExperiment: Experiment = {
    val maxTries = 10000
    val experiment = new Experiment
    var i = 1
    while (!experiment.complete && i < maxTries) {
      val mg = MarkedGrid(createValidStates)
      val at = if (mg.visualTarget) false else randomBoolean
      experiment.add(Trial(mg, at))
      i += 1
    }
    experiment
  }
  
  class ExperimentData {
    val nodes = new Array[mutable.MutableList[Int]](numOfStates)
    val targets = new mutable.MutableList[Int]
    val visualTargets = new mutable.MutableList[Int]
    val audioTargets = new mutable.MutableList[Int]
    val noneTargets = new mutable.MutableList[Int]
    for (i <- 0 to (nodes.size - 1)) nodes(i) = new mutable.MutableList[Int]

    override def toString: String = {
      val colSep = ","
      val newLine = "\n"
      val sb = new mutable.StringBuilder()
      def indexToPoint(i: Int) = Point(((i-1)/GRID.size)+1, if(i%GRID.size == 0) GRID.size else i%GRID.size)
      def processNodes() {
        var index: Int = 0
        def processNode(node: mutable.Iterable[Int]) {
          index += 1
          val point = indexToPoint(index)
          sb += 'N'
          sb.append(point.y)
          sb.append(point.x)
          sb ++= colSep
          sb ++= node.mkString(colSep)
          sb ++= newLine
        }
        nodes.foreach(processNode)
      }
      def processList(label: String, list: mutable.Iterable[Int]) {
        sb ++= label
        sb ++= colSep
        sb ++= list.mkString(colSep)
        sb ++= newLine
      }
      processNodes()
      processList("TRS", targets)
      processList("VTS", visualTargets)
      processList("ATS", audioTargets)
      processList("NTS", noneTargets)
      sb.toString()
    }
  }

  def experimentToData(experiment: Experiment): ExperimentData  = {
    val data = new ExperimentData
    def stateToInt(b: Boolean) = if (b) 2 else 1
    def booleanToInt(b: Boolean) = if (b) 1 else 0
    def transpose(states: States): Unit = for (i <- 0 to (numOfStates - 1)) {data.nodes(i) += stateToInt(states(i))}
    def add(trial: Trial): Unit = {
      transpose(trial.grid.states)
      data.targets += booleanToInt(trial.target)
      data.visualTargets += booleanToInt(trial.visualTarget)
      data.audioTargets += booleanToInt(trial.audioTarget)
      data.noneTargets += booleanToInt(! trial.target)
    }
    experiment.getTrials.foreach(add)
    data
  }

}

