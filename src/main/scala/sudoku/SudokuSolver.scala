package sudoku

object SudokuSolver {
  var a=0L

  private def internalSolve(state: State, depth: Int, curValue: Int): Option[State] ={
    val simplified = solveAllOnes(state)
    a=a+1; //if(a%100000==0 || depth<3)
     println(a+ " " + depth)
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

  def solveAllOnes(source: State):State = {
    var currentState = source
    var hasOne: Boolean = true
    while(!currentState.solved && !currentState.noMoves && hasOne) {
      val option = currentState.cellOptions
        .filter(x => x._2.size == 1)
        .headOption

      hasOne = option.isDefined
      if (hasOne) {
        val opt = option.get
        currentState = currentState.add(opt._1, opt._2.head)
      }
    }
    currentState
  }

  def solve(field: SudokuField): Option[State] = {
    val state = State(field)
    internalSolve(state, 0, 0)
  }
}
