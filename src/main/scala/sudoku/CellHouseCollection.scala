package sudoku

class CellHouseCollection(val horizontal: Array[Int], val vertical: Array[Int], val tile: Array[Int]) {
  lazy val merged: Set[Int] = (horizontal ++ vertical ++ tile).toSet
}
