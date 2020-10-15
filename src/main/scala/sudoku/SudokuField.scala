package sudoku

package object sudoku

import scala.collection.immutable.Iterable
import scala.collection.mutable.{Set => MSet}
import org.apache.commons.lang3.StringUtils

trait FieldMapperInterface {
  val rank: Int
  val sideLength: Int
  val size: Int
  val indexMap: Map[Int,CellDependency]
  val possibleValues: Set[Int]
  val maxValue: Int
  val mergedDependencies: Map[Int,Set[Int]]

  def create(input: Iterable[Int]): SudokuField
}

class FieldMapper(val rank: Int) extends FieldMapperInterface {
  require(rank>0)

  val sideLength: Int= math.pow(rank, 2).toInt
  val possibleValues: Set[Int] = (1 to sideLength).toSet
  lazy val maxValue: Int = possibleValues.max
  val size: Int = math.pow(sideLength, 2).toInt

  lazy val horizontalIndexes: Array[Array[Int]] = getHorizontalIndexes
  lazy val verticalIndexes: Array[Array[Int]] = getVerticalIndexes
  lazy val tileIndexes: Array[Array[Int]] = (0 until sideLength).map(p => getTileIndexes(p)).toArray
  lazy val indexMap: Map[Int,CellDependency] = (0 until size).map{ i =>
    i -> getCellDependency(i)
  }.toMap
  lazy val mergedDependencies: Map[Int,Set[Int]] = indexMap.map{ case (i, dependency) =>
    val indexes = (dependency.horizontal ++ dependency.vertical ++ dependency.tile).toSet
    i -> indexes
  }

  def create(input: Iterable[Int]): SudokuField = {
    require(input != null)
    val inputArray = input.toArray
    assert(inputArray.length == size, s"Length of input hast to be exactly ${size} but it is ${inputArray.length}")
    SudokuField(inputArray, inputArray.count(_==0), this)
  }

  private def getHorizontalIndexes:Array[Array[Int]] = {
    (0 until size by sideLength)
      .map(i => (0 until sideLength).map(j => i + j).toArray)
      .toArray
  }

  private def getVerticalIndexes:Array[Array[Int]] = {
    (0 until sideLength)
      .map(i => (0 until size by sideLength).map(j => i + j).toArray)
      .toArray
  }

  private def getTileIndexes(tileIndex: Int): Array[Int] = {
    val start = tileIndex/rank*(rank*sideLength) + tileIndex%rank * rank
    val end = start + (rank-1)*sideLength
    (start to end by sideLength)
      .flatten(i => i until i+rank)
      .toArray
  }

  private def getCellDependency(cellIndex: Int): CellDependency = {
    val horizontal = horizontalIndexes.find(h => h.contains(cellIndex)).get
    val vertical = verticalIndexes.find(v => v.contains(cellIndex)).get
    val tile = tileIndexes.find(v => v.contains(cellIndex)).get
    CellDependency(horizontal, vertical, tile)
  }
}

case class SudokuField(cells: Array[Int], freeCells: Int, mapper: FieldMapperInterface) {
  lazy val weights = getWeights

  private def getCells(indexes: Array[Int]): Array[Int] = indexes.map(i => cells(i))

  def getCellDependency(cellIndex: Int): CellDependency = {
    val dependencyIndex = mapper.indexMap(cellIndex)
    CellDependency(
      getCells(dependencyIndex.horizontal),
      getCells(dependencyIndex.vertical),
      getCells(dependencyIndex.tile))
  }

  def getOptions(cells: Array[Int]):Set[Int] = cells.filter(_>0).toSet

  def solved: Boolean = freeCells==0

  def getCellOptions(cellIndex: Int): Set[Int] = {
    val dependency = getCellDependency(cellIndex)

    val used : MSet[Int] = MSet()
    used.addAll(dependency.horizontal)
    used.addAll(dependency.vertical)
    used.addAll(dependency.tile)
    used.remove(0)

    mapper.possibleValues.filter(i => !used(i))
  }

  def getWeights: Array[Int] = {
    val acc: Array[Int] = Array.fill(mapper.sideLength+1)(0)
    cells.foreach(c => acc.update(c, acc(c)+1))
    acc
  }

  def getAllCellOptions: Map[Int, Set[Int]] =
    cells.zipWithIndex.filter(_._1==0)
      .map(p => p._2 -> getCellOptions(p._2))
      .toMap


  def add(cellIndex: Int, value: Int): SudokuField = {
    val newCells = cells.updated(cellIndex, value)
    SudokuField(newCells, freeCells-1, mapper)
  }

  def getRows(): Seq[Array[Int]] = {
    for{i <- 0 to mapper.size by mapper.sideLength}
      yield cells.slice(i, i+mapper.sideLength-1)
  }

  override def toString: String = {
    val maxVal = mapper.size - mapper.sideLength
    val len = mapper.maxValue.toString.length
    val space = StringUtils.repeat(' ', len)
    val sb = new StringBuilder()
    for(i <- 0 until mapper.size by mapper.sideLength) {
      for (j <- 0 until mapper.sideLength) {
        val c = cells(i+j)
        if(c==0) sb.append(space)
        else sb.append(StringUtils.leftPad(c.toString, len))
      }
      if(i < maxVal) {
        sb.append("\n")
      }
    }
    sb.toString
  }
}