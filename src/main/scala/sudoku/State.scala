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
  def getComplexity:Double = cellOptions.foldLeft(1D)((acc, p) => acc*p._2.size)

  def getMoves(cellIndex: Int): LazyList[(Int, State)] = {
    val options = cellOptions(cellIndex)
    options.to(LazyList).map(p => (p, add(cellIndex, p)))
  }

  def add(cellIndex: Int, value: Int): State = {
    val cellDependencies = sudokuField.mapper.mergedDependencies(cellIndex)
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
}
