package sudoku

object Main {
  def main(args: Array[String]): Unit = {
    val input: Array[Int] =
      Array(
        0, 2, 0, 0,  1,16, 4, 0,  0,10, 0, 0,  0, 0,12, 0,
        10, 0, 0,11,  0, 6, 0, 0,  2, 0, 0, 0,  0,14, 0, 0,
        0,12, 0, 0,  0, 0, 0, 8,  0, 0, 3, 0,  2, 0, 0,10,
        0, 0, 0,15,  0, 0, 5, 0,  1, 0, 0, 0,  0, 0, 4, 0,

        11, 0,12, 0,  0,14, 0, 6,  0, 1, 0, 3,  0,10, 0, 4,
        0,13, 0, 7, 10, 0, 3, 0,  0, 0, 0,16,  0, 0, 5, 0,
        16, 0,15, 0, 12, 0, 0, 0,  8, 0, 0, 0,  9, 0, 0, 2,
        0, 0, 0, 9,  0,11, 0, 0,  0, 0, 0,15,  0, 3, 0, 0,

        0, 8, 4, 0, 11, 0,12, 2,  0, 5, 0, 0, 14, 0, 9, 0,
        7, 0,10, 0,  0, 0, 0, 0,  3, 0, 0, 9,  0,15, 0,11,
        0, 6, 0, 0, 15, 0, 0,16,  0, 0,10, 0, 12, 0,13, 0,
        0, 0, 0,12,  0, 1, 0, 0,  0,16, 0,13,  0, 6, 0, 7,

        0, 0, 0, 0,  0, 0, 0, 0, 11, 0, 5, 0,  0, 0, 0, 0,
        0, 0, 0,10, 13, 9, 0, 5,  0,12, 0, 0, 15, 0, 8, 0,
        0, 7, 0, 0,  8, 0,14, 0,  0, 0,15, 0,  0, 0, 0, 0,
        14, 0, 3, 0,  0, 7, 0, 0,  0, 0,13, 2,  0, 4, 0,16)

    val mapper = new FieldMapper(4)
    val field = mapper.create(input)
    println(field.toString)
    println("-- Solution --")
    val result = SudokuSolver.solve(field)
    println(result.get.sudokuField.toString)
  }
}
