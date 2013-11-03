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

object Generator {

  type States = List[Boolean]

  val numOfVisualTargets = 25
  val numOfAudioTargets = 25
  val numOfNonTargets = 25
  val numOfTrials = numOfVisualTargets + numOfAudioTargets + numOfNonTargets

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

  def randomBoolean: Boolean = Math.random() > 0.5

  def createStates: States = ( for(i <- 1 to numOfStates) yield randomBoolean ).toList

  def validStates(states: States): Boolean = { val s = states.map(if (_) 1 else 0).sum; s >= 6 && s <= 10}

  def createValidStates: States = {val states = createStates; if (validStates(states)) states else createValidStates}

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
    val nodes = new Array[mutable.MutableList[Boolean]](numOfStates)
    val visualTargets = new mutable.MutableList[Boolean]
    val audioTargets = new mutable.MutableList[Boolean]
    val noneTargets = new mutable.MutableList[Boolean]
    for (i <- 0 to (nodes.size - 1)) nodes(i) = new mutable.MutableList[Boolean]

    override def toString: String = {
      val colSep = ";"
      val newLine = "\n"
      val sb = new mutable.StringBuilder()
      def indexToPoint(i: Int) = Point(((i-1)/GRID.size)+1, if(i%GRID.size == 0) GRID.size else i%GRID.size)
      def processNodes() {
        var index: Int = 0
        def processNode(node: mutable.MutableList[Boolean]) {
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
      def processList(label: String, list: mutable.MutableList[Boolean]) {
        sb ++= label
        sb ++= colSep
        sb ++= list.mkString(colSep)
        sb ++= newLine
      }
      processNodes()
      processList("VTS", visualTargets)
      processList("ATS", audioTargets)
      processList("NTS", noneTargets)
      sb.toString()
    }
  }

  def experimentToData(experiment: Experiment): ExperimentData  = {
    val data = new ExperimentData
    def transpose(states: States): Unit = for (i <- 0 to (numOfStates - 1)) {data.nodes(i) += states(i)}
    def add(trial: Trial): Unit = {
      transpose(trial.grid.states)
      data.visualTargets += trial.visualTarget
      data.audioTargets += trial.audioTarget
      data.noneTargets += (! trial.target)
    }
    experiment.getTrials.foreach(add)
    data
  }

}

