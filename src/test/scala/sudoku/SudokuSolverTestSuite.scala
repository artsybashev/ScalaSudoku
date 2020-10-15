package sudoku

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class SudokuSolverTestSuite extends AnyFlatSpec {
  lazy val mapper2 = new FieldMapper(2)
  lazy val mapper3 = new FieldMapper(3)
  lazy val mapper4 = new FieldMapper(4)

  "Simple 4x4 puzzle" should "be solved" in {
    val input: Array[Int] =
      Array(
        0,2,4,0,
        1,0,0,3,
        4,0,0,2,
        0,1,3,0)
    val expected: Array[Int] =
      Array(
        3,2,4,1,
        1,4,2,3,
        4,3,1,2,
        2,1,3,4)

    val result = SudokuSolver.solve(mapper2.create(input))
    result.isDefined shouldBe true
    val r = result.get
    r.solved should equal(true)
    r.sudokuField.cells should equal(expected)
  }

  "Simple 9x9 puzzle" should "be solved" in {
    val input: Array[Int] =
      Array(
        0,0,0, 2,6,0, 7,0,1,
        6,8,0, 0,7,0, 0,9,0,
        1,9,0, 0,0,4, 5,0,0,

        8,2,0, 1,0,0, 0,4,0,
        0,0,4, 6,0,2, 9,0,0,
        0,5,0, 0,0,3, 0,2,8,

        0,0,9, 3,0,0, 0,7,4,
        0,4,0, 0,5,0, 0,3,6,
        7,0,3, 0,1,8, 0,0,0)
    val expected: Array[Int] =
      Array(
        4,3,5, 2,6,9, 7,8,1,
        6,8,2, 5,7,1, 4,9,3,
        1,9,7, 8,3,4, 5,6,2,
        8,2,6, 1,9,5, 3,4,7,
        3,7,4, 6,8,2, 9,1,5,
        9,5,1, 7,4,3, 6,2,8,
        5,1,9, 3,2,6, 8,7,4,
        2,4,8, 9,5,7, 1,3,6,
        7,6,3, 4,1,8, 2,5,9)

    val result = SudokuSolver.solve(mapper3.create(input))
    result.isDefined shouldBe true
    val r = result.get
    r.solved should equal(true)
    r.sudokuField.cells should equal(expected)
  }

  "Medium 9x9 puzzle" should "be solved" in {
    val input: Array[Int] =
      Array(
        0,3,0, 7,0,8, 0,0,0,
        0,0,0, 0,0,0, 2,0,0,
        0,0,4, 0,0,0, 0,7,1,

        6,0,1, 0,0,0, 0,0,2,
        7,0,0, 0,0,5, 0,0,0,
        0,0,0, 0,0,0, 8,5,0,

        0,0,3, 0,8,0, 0,0,0,
        9,2,0, 0,6,0, 0,0,5,
        1,0,0, 0,0,0, 9,0,6)
    val expected: Array[Int] =
      Array(
        2,3,6, 7,1,8, 5,9,4,
        5,1,7, 9,3,4, 2,6,8,
        8,9,4, 2,5,6, 3,7,1,
        6,5,1, 8,9,3, 7,4,2,
        7,8,9, 4,2,5, 6,1,3,
        3,4,2, 6,7,1, 8,5,9,
        4,6,3, 5,8,9, 1,2,7,
        9,2,8, 1,6,7, 4,3,5,
        1,7,5, 3,4,2, 9,8,6)

    val result = SudokuSolver.solve(mapper3.create(input))
    result.isDefined shouldBe true
    val r = result.get
    r.solved should equal(true)
    r.sudokuField.cells should equal(expected)
  }

  "Hard 9x9 puzzle" should "be solved" in {
    val input: Array[Int] =
      Array(
        0,0,0, 8,0,1, 0,0,0,
        0,0,0, 0,0,0, 0,4,3,
        5,0,0, 0,0,0, 0,0,0,

        0,0,0, 0,7,0, 8,0,0,
        0,0,0, 0,0,0, 1,0,0,
        0,2,0, 0,3,0, 0,5,0,

        6,0,0, 0,0,0, 0,7,5,
        0,0,3, 4,0,0, 0,0,0,
        0,0,0, 2,0,0, 6,0,0)
    val expected: Array[Int] =
      Array(
        2,3,7, 8,4,1, 5,6,9,
        1,8,6, 7,9,5, 2,4,3,
        5,9,4, 3,2,6, 7,1,8,

        3,1,5, 6,7,4, 8,9,2,
        4,6,9, 5,8,2, 1,3,7,
        7,2,8, 1,3,9, 4,5,6,

        6,4,2, 9,1,8, 3,7,5,
        8,5,3, 4,6,7, 9,2,1,
        9,7,1, 2,5,3, 6,8,4)

    val result = SudokuSolver.solve(mapper3.create(input))
    result.isDefined shouldBe true
    val r = result.get
    r.solved should equal(true)
    r.sudokuField.cells should equal(expected)
  }

  "Extra hard 9x9 puzzle" should "be solved" in {
    val input: Array[Int] =
      Array(
        8,0,0, 0,0,0, 0,0,0,
        0,0,3, 6,0,0, 0,0,0,
        0,7,0, 0,9,0, 2,0,0,

        0,5,0, 0,0,7, 0,0,0,
        0,0,0, 0,4,5, 7,0,0,
        0,0,0, 1,0,0, 0,3,0,

        0,0,1, 0,0,0, 0,6,8,
        0,0,8, 5,0,0, 0,1,0,
        0,9,0, 0,0,0, 4,0,0)
    val expected: Array[Int] =
      Array(
        8,1,2, 7,5,3, 6,4,9,
        9,4,3, 6,8,2, 1,7,5,
        6,7,5, 4,9,1, 2,8,3,

        1,5,4, 2,3,7, 8,9,6,
        3,6,9, 8,4,5, 7,2,1,
        2,8,7, 1,6,9, 5,3,4,

        5,2,1, 9,7,4, 3,6,8,
        4,3,8, 5,2,6, 9,1,7,
        7,9,6, 3,1,8, 4,5,2)

    val result = SudokuSolver.solve(mapper3.create(input))
    result.isDefined shouldBe true
    val r = result.get
    r.solved should equal(true)
    r.sudokuField.cells should equal(expected)
  }

  "Extremely hard 16x16 puzzle" should "be solved" ignore { //it has insane complexity
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
    val expected: Array[Int] =
      Array(
        8,1,2, 7,5,3, 6,4,9,
        9,4,3, 6,8,2, 1,7,5,
        6,7,5, 4,9,1, 2,8,3,

        1,5,4, 2,3,7, 8,9,6,
        3,6,9, 8,4,5, 7,2,1,
        2,8,7, 1,6,9, 5,3,4,

        5,2,1, 9,7,4, 3,6,8,
        4,3,8, 5,2,6, 9,1,7,
        7,9,6, 3,1,8, 4,5,2)

    val result = SudokuSolver.solve(mapper4.create(input))
    result.isDefined shouldBe true
    val r = result.get
    r.solved should equal(true)
    r.sudokuField.cells should equal(expected)
  }

  "Easy 16x16 puzzle" should "be solved" in {
    val input: Array[Int] =
      Array(
         0,11, 9, 0,  0,16,13, 4,  0, 0,14, 0, 10, 6,15, 0,
         4,12,15, 0,  3, 6, 0,11,  0, 5, 0, 1, 16, 7,14, 2,
         1, 0, 6, 0, 15, 2, 0, 0, 11, 9,10, 0,  0, 0, 8, 0,
         0,13, 0, 0,  0, 1, 0, 0,  4, 6, 0,15 , 0, 0, 0, 0,

         0, 0, 0, 0,  0, 0,15, 0,  8, 1, 5, 3,  0, 4,11, 7,
         6, 0, 1, 0,  0,12, 8, 0,  9, 0, 0, 2,  0, 0, 3, 0,
        14, 0, 4,13,  6, 0, 0, 3,  0,12, 7,10,  8, 0, 2, 0,
         3, 8, 0, 0,  4, 7, 2, 0,  6, 0, 0, 0,  0,12,16, 5,

        13, 0, 0,16,  0, 8,14,10,  3, 4,15, 0, 12, 5, 1,11,
         0, 0, 0, 6,  2, 0, 0, 1, 10, 0,11, 0, 15, 3, 0, 9,
         7, 0, 0,12,  0, 4, 0,15,  5, 0, 9,14,  0, 0, 0, 0,
        10, 0, 0, 8,  0, 0,11, 0,  0, 0, 1,12,  4, 0,13,16,

         0, 0, 0, 0,  0, 0, 7, 0, 15, 2, 0, 0, 0, 0,12, 3,
         0, 0, 7, 0,  0,10, 6, 0,  1, 8, 0,13,11, 0, 9,14,
         8, 6, 5, 0,  0, 3, 0, 0, 14, 0, 0, 9, 0, 0, 0, 0,
         0,16, 0, 2,  0, 0, 0,14,  0,10, 0, 0, 0, 0, 0, 0)
    val expected: Array[Int] =
      Array(
         2,11, 9, 5,  8,16,13, 4, 12, 3,14, 7, 10, 6,15, 1,
         4,12,15,10,  3, 6, 9,11, 13, 5, 8, 1, 16, 7,14, 2,
         1,14, 6, 7, 15, 2, 5,12, 11, 9,10,16,  3,13, 8, 4,
        16,13, 8, 3, 14, 1,10, 7,  4, 6, 2,15,  9,11, 5,12,

        12, 2,16, 9, 10,14,15,13,  8, 1, 5, 3,  6, 4,11, 7,
         6, 7, 1,11,  5,12, 8,16,  9,15, 4, 2, 14,10, 3,13,
        14, 5, 4,13,  6,11, 1, 3, 16,12, 7,10,  8, 9, 2,15,
         3, 8,10,15,  4, 7, 2, 9,  6,14,13,11,  1,12,16, 5,

        13, 9, 2,16,  7, 8,14,10,  3, 4,15, 6, 12, 5, 1,11,
         5, 4,14, 6,  2,13,12, 1, 10,16,11, 8, 15, 3, 7, 9,
         7, 1,11,12, 16, 4, 3,15,  5,13, 9,14,  2, 8,10, 6,
        10,15, 3, 8,  9, 5,11, 6,  2, 7, 1,12,  4,14,13,16,

        11,10,13,14,  1, 9, 7, 8, 15, 2, 6, 4,  5,16,12, 3,
        15, 3, 7, 4, 12,10, 6, 5,  1, 8,16,13, 11, 2, 9,14,
         8, 6, 5, 1, 13, 3,16, 2, 14,11,12, 9,  7,15, 4,10,
         9,16,12, 2, 11,15, 4,14,  7,10, 3, 5, 13, 1, 6, 8)

    val result = SudokuSolver.solve(mapper4.create(input))
    result.isDefined shouldBe true
    val r = result.get
    r.solved should equal(true)
    r.sudokuField.cells should equal(expected)
  }
}
