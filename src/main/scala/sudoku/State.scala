package sudoku

import scala.collection.immutable

object State {
  def apply(sudokuField: SudokuField): State = {
    new State(sudokuField, sudokuField.getAllCellOptions)
  }
}

case class State(sudokuField: SudokuField, cellOptions: Map[Int, CellOption]) {
  lazy val complexity:Double = getComplexity
  lazy val solved: Boolean = sudokuField.solved
  lazy val noMoves: Boolean = cellOptions.isEmpty
  val ruleProvider: RuleProviderInterface = sudokuField.ruleProvider

  def getComplexity:Double = cellOptions.foldLeft(1D)((acc, p) => acc*p._2.size)

  def getMoves(cellIndex: Int): LazyList[(Int, State)] = {
    val options = cellOptions(cellIndex)
    options.to(LazyList).map(p => (p, add(cellIndex, p)))
  }

  def add(cellIndex: Int, value: Int): State = {
    val cellDependencies = sudokuField.ruleProvider.houseMap(cellIndex).merged
    val optionBuilder = collection.mutable.Map.newBuilder[Int, CellOption]
    optionBuilder.addAll(cellOptions)
    val options = optionBuilder.result()
    options.remove(cellIndex)
    cellDependencies.foreach(i => removeOptionsValue(options, i, value))

    new State(sudokuField.add(cellIndex, value), options.toMap)
  }

  private def removeOptionsValue(options: collection.mutable.Map[Int, CellOption], cellIndex: Int, value: Int): Unit = {
    val oValues = options.get(cellIndex)
    if(oValues.isDefined) {
      val values = oValues.get
      if(values.contains(value)) {
        val newOh = values - value
        if(newOh.isEmpty) options.remove(cellIndex)
        else options(cellIndex) = newOh
      }
    }
  }

  private def getHouseOptions(cellCollection: CellHouseCollection): (Map[Int,CellOption],Map[Int,CellOption],Map[Int,CellOption]) = {
    val hMapBuilder = collection.immutable.Map.newBuilder[Int, CellOption]
    val vMapBuilder = collection.immutable.Map.newBuilder[Int, CellOption]
    val tMapBuilder = collection.immutable.Map.newBuilder[Int, CellOption]
    for(i <- 0 until sudokuField.ruleProvider.sideLength){
      val h = cellCollection.horizontal(i)
      val v = cellCollection.vertical(i)
      val t = cellCollection.tile(i)
      val hOption = cellOptions.get(h)
      val vOption = cellOptions.get(v)
      val tOption = cellOptions.get(t)
      if(hOption.isDefined) hMapBuilder.addOne(h,hOption.get)
      if(vOption.isDefined) vMapBuilder.addOne(v,vOption.get)
      if(tOption.isDefined) tMapBuilder.addOne(t,tOption.get)
    }
    (hMapBuilder.result, vMapBuilder.result, tMapBuilder.result)
  }

  private def findConjugated(options: Map[Int,CellOption], cellOptions: CellOption):Map[Int,CellOption] = {
      /*val sampleSize = cellOptions.size
      var exactNum = 0
      val builder = collection.immutable.Map.newBuilder[Int,CellOption]
      for(item <- options) {
        val currentSet = item._2
        val currentSetSize = currentSet.size
        val diff = currentSet -- cellOptions
        if(diff.size<currentSetSize) {
          if(diff.size==0 && currentSetSize==sampleSize) exactNum += 1
          else builder.addOne(item._1, diff)
        }
      }
    if(exactNum==sampleSize) builder.result else Map.empty[Int,CellOption]
*/
    val (exact, others) = options.partition(p => p._2.equals(cellOptions))
    if(exact.size==cellOptions.size)
      others
        .map(p => (p._1, (p._2 -- cellOptions), p._2.size))
        .filter(p => p._2.nonEmpty && p._2.size < p._3)
        .map(p => (p._1, p._2))
        .toMap
    else Map.empty[Int,CellOption]
  }

  def removeConjugatedInHouses(): State = {
    val maxSize = ruleProvider.maxValue/2
    val updatesMapBuilder = collection.immutable.Map.newBuilder[Int, CellOption]
    for(co <- cellOptions.filter(p => p._2.size>1 && p._2.size<=maxSize)) {
      val (h,v,t) = getHouseOptions(ruleProvider.houseMap(co._1))
      val hUpdates = findConjugated(h, co._2)
      if(hUpdates.nonEmpty) updatesMapBuilder.addAll(hUpdates)
      val vUpdates = findConjugated(v, co._2)
      if(vUpdates.nonEmpty) updatesMapBuilder.addAll(vUpdates)
      val tUpdates = findConjugated(t, co._2)
      if(tUpdates.nonEmpty) updatesMapBuilder.addAll(tUpdates)
    }
    val updatesMap = updatesMapBuilder.result()
    if(updatesMap.isEmpty) this
    else {
      new State(sudokuField, cellOptions.map(p => (p._1, updatesMap.getOrElse(p._1, p._2))))
    }
  }

  def removeGlobalUniques(): State = {
    removeUniquesInGroup(this, cellOptions)
  }

  def removeUniques(): State = {
    val maps = cellOptions.map(co => (sudokuField.ruleProvider.cellCoordinates(co._1), co))
    val hg = maps.groupBy(h => h._1._1).filter(_._2.size>1).map(_._2.map(p => p._2))
    val vg = maps.groupBy(h => h._1._2).filter(_._2.size>1).map(_._2.map(p => p._2))
    val tg = maps.groupBy(h => h._1._3).filter(_._2.size>1).map(_._2.map(p => p._2))
    val g: immutable.Iterable[Map[Int, CellOption]] = hg++vg++tg

    var current = this
    for(item <- g) {
      current = removeUniquesInGroup(current, item)
    }
    current
  }

  def removeUniquesInGroup(source:State, options: Iterable[(Int, CellOption)]): State = {
    val tracked = Array.fill(sudokuField.ruleProvider.sideLength+1)(-1)
    for(co <- options) {
      for(v <- co._2) {
        if(tracked(v) == -1) tracked.update(v,co._1)
        else tracked.update(v,-2)
      }
    }

    var current = source
    for(value <- 1 to sudokuField.ruleProvider.sideLength) {
      val index = tracked(value)
      if(index >= 0) current = add(index, value)
    }
    current
  }
}
