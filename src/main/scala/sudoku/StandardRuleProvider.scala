package sudoku

import scala.collection.immutable.{BitSet, Iterable}

trait RuleProviderInterface {
  val rank: Int
  val sideLength: Int
  val size: Int
  val houseMap: Map[Int,CellHouseCollection]
  val possibleValues: CellOption
  val maxValue: Int
  val cellCoordinates: Map[Int, (Int, Int, Int)]

  def create(input: Iterable[Int]): SudokuField
}

class StandardRuleProvider(val rank: Int) extends RuleProviderInterface {
  require(rank > 0)

  val sideLength: Int = rank*rank
  val possibleValues: CellOption = BitSet.fromSpecific(1 to sideLength)
  lazy val maxValue: Int = possibleValues.max
  val size: Int = sideLength*sideLength

  lazy val horizontalIndexes: Array[Array[Int]] = getHorizontalIndexes
  lazy val verticalIndexes: Array[Array[Int]] = getVerticalIndexes
  lazy val tileIndexes: Array[Array[Int]] = (0 until sideLength).map(p => getTileIndexes(p)).toArray
  lazy val houseMap: Map[Int, CellHouseCollection] = (0 until size).map { i =>
    i -> getCellDependency(i)
  }.toMap
  lazy val cellCoordinates: Map[Int, (Int, Int, Int)] = (0 until size).map { i =>
    i -> getCellCoordinates(i)
  }.toMap

  def create(input: Iterable[Int]): SudokuField = {
    require(input != null)
    val inputArray = input.toArray
    assert(inputArray.length == size, s"Length of input hast to be exactly ${size} but it is ${inputArray.length}")
    new SudokuField(inputArray, inputArray.count(_ == 0), this)
  }

  private def getHorizontalIndexes: Array[Array[Int]] = {
    (0 until size by sideLength)
      .map(i => (0 until sideLength).map(j => i + j).toArray)
      .toArray
  }

  private def getVerticalIndexes: Array[Array[Int]] = {
    (0 until sideLength)
      .map(i => (0 until size by sideLength).map(j => i + j).toArray)
      .toArray
  }

  private def getTileIndexes(tileIndex: Int): Array[Int] = {
    val start = tileIndex / rank * (rank * sideLength) + tileIndex % rank * rank
    val end = start + (rank - 1) * sideLength
    (start to end by sideLength)
      .flatten(i => i until i + rank)
      .toArray
  }

  private def getCellDependency(cellIndex: Int): CellHouseCollection = {
    val horizontal = horizontalIndexes.find(h => h.contains(cellIndex)).get
    val vertical = verticalIndexes.find(v => v.contains(cellIndex)).get
    val tile = tileIndexes.find(v => v.contains(cellIndex)).get
    new CellHouseCollection(horizontal, vertical, tile)
  }

  private def getCellCoordinates(cellIndex: Int): (Int, Int, Int) = {
    val horizontal = horizontalIndexes.zipWithIndex.find(h => h._1.contains(cellIndex)).get._2
    val vertical = verticalIndexes.zipWithIndex.find(v => v._1.contains(cellIndex)).get._2
    val tile = tileIndexes.zipWithIndex.find(v => v._1.contains(cellIndex)).get._2
    (horizontal, vertical, tile)
  }
}
