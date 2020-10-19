package sudoku

object State {
  def apply(sudokuField: SudokuField): State = {
    new State(sudokuField, sudokuField.getAllCellOptions)
  }
}

case class State(sudokuField: SudokuField, cellOptions: Map[Int, Set[Int]]) {
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
    val optionBuilder = collection.mutable.Map.newBuilder[Int, Set[Int]]
    optionBuilder.addAll(cellOptions)
    val options = optionBuilder.result()
    options.remove(cellIndex)
    cellDependencies.foreach(i => removeOptionsValue(options, i, value))

    new State(sudokuField.add(cellIndex, value), options.toMap)
  }

  private def removeOptionsValue(options: collection.mutable.Map[Int, Set[Int]], cellIndex: Int, value: Int): Unit = {
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

  private def getHouseOptions(cellCollection: CellHouseCollection): (Map[Int,Set[Int]],Map[Int,Set[Int]],Map[Int,Set[Int]]) = {
    val hMapBuilder = collection.immutable.Map.newBuilder[Int, Set[Int]]
    val vMapBuilder = collection.immutable.Map.newBuilder[Int, Set[Int]]
    val tMapBuilder = collection.immutable.Map.newBuilder[Int, Set[Int]]
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

  private def findConjugated(options: Map[Int,Set[Int]], cellIndex: Int, cellOptions: Set[Int]):Map[Int,Set[Int]] = {
    val (exact, others) = options.partition(p => p._2 == cellOptions)
    if(exact.size==cellOptions.size)
      others
        .map(p => (p._1, p._2.diff(cellOptions), p._2.size))
        .filter(p => !p._2.isEmpty && p._2.size < p._3)
        .map(p => (p._1, p._2))
        .toMap
    else Map.empty[Int,Set[Int]]
  }

  def removeConjugatedInHouses: State = {
    val used = collection.mutable.Set[Int]()
    val maxSize = ruleProvider.maxValue/2
    val updatesMapBuilder = collection.immutable.Map.newBuilder[Int, Set[Int]]
    for(co <- cellOptions.filter(p => p._2.size>1 && p._2.size<=maxSize)) {
      used.addOne(co._1)
      val (h,v,t) = getHouseOptions(ruleProvider.houseMap(co._1))
      val hUpdates = findConjugated(h, co._1, co._2)
      if(!hUpdates.isEmpty) updatesMapBuilder.addAll(hUpdates)
      val vUpdates = findConjugated(v, co._1, co._2)
      if(!vUpdates.isEmpty) updatesMapBuilder.addAll(vUpdates)
      val tUpdates = findConjugated(t, co._1, co._2)
      if(!tUpdates.isEmpty) updatesMapBuilder.addAll(tUpdates)
    }
    val updatesMap = updatesMapBuilder.result()
    if(updatesMap.isEmpty) this
    else {
      new State(sudokuField, cellOptions.map(p => (p._1, updatesMap.getOrElse(p._1, p._2))))
    }
  }

  def removeGlobalUniques: State = {
    val tracked = Array.fill(sudokuField.ruleProvider.sideLength+1)(-1)
    for(co <- cellOptions) {
      for(v <- co._2) {
        if(tracked(v) == -1) tracked.update(v,co._1)
        else tracked.update(v,-2)
      }
    }
    var current = this
    for(value <- 1 to sudokuField.ruleProvider.sideLength) {
      val index = tracked(value)
      if(index >= 0) current = add(index, value)
    }
    current
  }
}
