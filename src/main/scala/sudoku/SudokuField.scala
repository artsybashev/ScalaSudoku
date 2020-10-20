package sudoku

import scala.collection.mutable.{Set => MSet}
import org.apache.commons.lang3.StringUtils

class SudokuField(val cells: Array[Int], val freeCells: Int, val ruleProvider: RuleProviderInterface) {
  lazy val weights = getWeights

  private def getCells(indexes: Array[Int]): Array[Int] = indexes.map(i => cells(i))

  def getCellDependency(cellIndex: Int): CellHouseCollection = {
    val dependencyIndex = ruleProvider.houseMap(cellIndex)
    new CellHouseCollection(
      getCells(dependencyIndex.horizontal),
      getCells(dependencyIndex.vertical),
      getCells(dependencyIndex.tile))
  }

  def getMergedCellDependency(cellIndex: Int): Set[Int] = {
    ruleProvider.houseMap(cellIndex).merged.map(i => cells(i))
  }

  def getOptions(cells: Array[Int]):Set[Int] = cells.filter(_>0).toSet

  def solved: Boolean = freeCells==0

  def getCellOptions(cellIndex: Int): CellOption = {
    val dependency = getCellDependency(cellIndex)

    val used : MSet[Int] = MSet()
    used.addAll(dependency.horizontal)
    used.addAll(dependency.vertical)
    used.addAll(dependency.tile)
    used.remove(0)

    ruleProvider.possibleValues.filter(i => !used(i))
  }

  def getWeights: Array[Int] = {
    val acc: Array[Int] = Array.fill(ruleProvider.sideLength+1)(0)
    cells.foreach(c => acc.update(c, acc(c)+1))
    acc
  }

  def getAllCellOptions: Map[Int, CellOption] =
    cells.zipWithIndex.filter(_._1==0)
      .map(p => p._2 -> getCellOptions(p._2))
      .toMap


  def add(cellIndex: Int, value: Int): SudokuField = {
    val newCells = cells.updated(cellIndex, value)
    new SudokuField(newCells, freeCells-1, ruleProvider)
  }

  def getRows(): Seq[Array[Int]] = {
    for{i <- 0 to ruleProvider.size by ruleProvider.sideLength}
      yield cells.slice(i, i+ruleProvider.sideLength-1)
  }

  override def toString: String = {
    val lastRowIndex = ruleProvider.size - ruleProvider.sideLength
    val len = ruleProvider.maxValue.toString.length
    val space = StringUtils.repeat(' ', len)
    val sb = new StringBuilder()
    for(i <- 0 until ruleProvider.size by ruleProvider.sideLength) {
      for (j <- 0 until ruleProvider.sideLength) {
        val c = cells(i+j)
        if(c==0) sb.append(space)
        else sb.append(StringUtils.leftPad(c.toString, len))
      }
      if(i<lastRowIndex) sb.append("\n")
    }
    sb.toString
  }
}

