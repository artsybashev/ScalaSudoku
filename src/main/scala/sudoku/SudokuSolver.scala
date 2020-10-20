package sudoku

object SudokuSolver {
  def solveSingles(source: State):State = {
    var currentState = source
    var hasReductions: Boolean = true
    while(!currentState.solved && !currentState.noMoves && hasReductions) {
      val singleResult = tryReduceSingle(currentState)
      if (singleResult._1) {
        currentState = singleResult._2
        hasReductions = singleResult._1
      } else {
        val uniqueResult = tryReduceUnique(currentState)
        if (uniqueResult._1) {
          currentState = uniqueResult._2
          hasReductions = uniqueResult._1
        } else {
          val cResult = tryReduceConjugated(currentState)
          if (cResult._1) {
            currentState = cResult._2
            hasReductions = cResult._1
          } else hasReductions = false
        }
      }
    }
    currentState
  }

  def tryReduceSingle(source: State): (Boolean, State) = {
    val option = source.cellOptions.find(_._2.size == 1)
    if (option.isDefined) {
      val opt = option.get
      (true, source.add(opt._1, opt._2.head))
    } else (false, source)
  }

  def tryReduceUnique(source: State): (Boolean, State) = {
    val newState = source.removeUniques
    (!(newState eq source), newState)
  }
  def tryReduceConjugated(source: State): (Boolean, State) = {
    val newState = source.removeConjugatedInHouses
    (!(newState eq source), newState)
  }

  def solve(field: SudokuField): Option[State] = {
    var iterationCounter=0L

    def internalSolve(state: State, depth: Int, curValue: Int): Option[State] ={
      val simplified = solveSingles(state)
      iterationCounter=iterationCounter+1
      if(iterationCounter%100000==0 || depth<5)
        println(iterationCounter+ " " + depth)
      if(simplified.solved) Some(simplified)
      else if(simplified.noMoves) None
      else {

        val smallest = simplified
          .cellOptions
          .toSeq
          .sortBy(p => p._2.size)
          .head

        val moves = simplified.getMoves(smallest._1)

        moves
          .sortBy(_._2.complexity)
          .map(x => internalSolve(x._2, depth+1,x._1))
          .filter(_.isDefined)
          .map(_.get)
          .headOption
      }
    }

    val state = State(field)
    internalSolve(state, 0, 0)
  }
}
