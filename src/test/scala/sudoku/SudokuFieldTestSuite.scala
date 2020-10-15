package sudoku

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class SudokuFieldTestSuite extends AnyFlatSpec {
  lazy val expectedHorizontalIndexes: Array[Array[Int]] =
    Array(
      Array( 0, 1, 2, 3, 4, 5, 6, 7, 8),
      Array( 9,10,11,12,13,14,15,16,17),
      Array(18,19,20,21,22,23,24,25,26),
      Array(27,28,29,30,31,32,33,34,35),
      Array(36,37,38,39,40,41,42,43,44),
      Array(45,46,47,48,49,50,51,52,53),
      Array(54,55,56,57,58,59,60,61,62),
      Array(63,64,65,66,67,68,69,70,71),
      Array(72,73,74,75,76,77,78,79,80)
    )

  lazy val expectedVerticalIndexes: Array[Array[Int]] =
    Array(
      Array(0,  9, 18, 27, 36, 45, 54, 63, 72),
      Array(1, 10, 19, 28, 37, 46, 55, 64, 73),
      Array(2, 11, 20, 29, 38, 47, 56, 65, 74),
      Array(3, 12, 21, 30, 39, 48, 57, 66, 75),
      Array(4, 13, 22, 31, 40, 49, 58, 67, 76),
      Array(5, 14, 23, 32, 41, 50, 59, 68, 77),
      Array(6, 15, 24, 33, 42, 51, 60, 69, 78),
      Array(7, 16, 25, 34, 43, 52, 61, 70, 79),
      Array(8, 17, 26, 35, 44, 53, 62, 71, 80))

  lazy val expectedTileIndexes: Array[Array[Int]] = Array(
    Array( //0
       0, 1, 2,
       9,10,11,
      18,19,20),
    Array( //1
       3, 4, 5,
      12,13,14,
      21,22,23),
    Array( //2
       6, 7, 8,
      15,16,17,
      24,25,26),
    Array( //3
      27,28,29,
      36,37,38,
      45,46,47),
    Array( //4
      30,31,32,
      39,40,41,
      48,49,50),
    Array( //5
      33,34,35,
      42,43,44,
      51,52,53),
    Array( //6
      54,55,56,
      63,64,65,
      72,73,74),
    Array( //7
      57,58,59,
      66,67,68,
      75,76,77),
    Array( //8
      60,61,62,
      69,70,71,
      78,79,80)
  )

  "horizontalIndexes" should "be exactly" in {
    val horizontalIndexes = new FieldMapper(3).horizontalIndexes
    horizontalIndexes should equal(expectedHorizontalIndexes)
  }

  "verticalIndexes" should "be exactly" in {
    val verticalIndexes = new FieldMapper(3).verticalIndexes
    verticalIndexes should equal(expectedVerticalIndexes)
  }

  "tileIndexes" should "be exactly" in {
    val tileIndexes = new FieldMapper(3).tileIndexes
    tileIndexes should equal(expectedTileIndexes)
  }

  "indexMap cells" should "has correct horizontal" in {
    val indexMap = new FieldMapper(3).indexMap
    val dependency0 = indexMap.get(0).get
    dependency0.horizontal should equal(expectedHorizontalIndexes(0))

    val dependency5 = indexMap.get(5).get
    dependency5.horizontal should equal(expectedHorizontalIndexes(0))

    val dependency75 = indexMap.get(75).get
    dependency75.horizontal should equal(expectedHorizontalIndexes(8))
  }

  "indexMap" should "has correct vertical" in {
    val indexMap = new FieldMapper(3).indexMap
    val dependency0 = indexMap.get(0).get
    dependency0.vertical should equal(expectedVerticalIndexes(0))

    val dependency5 = indexMap.get(5).get
    dependency5.vertical should equal(expectedVerticalIndexes(5))

    val dependency75 = indexMap.get(75).get
    dependency75.vertical should equal(expectedVerticalIndexes(3))
  }

  "indexMap" should "has correct tiles" in {
    val indexMap = new FieldMapper(3).indexMap
    val dependency0 = indexMap.get(0).get
    dependency0.tile should equal(expectedTileIndexes(0))

    val dependency5 = indexMap.get(5).get
    dependency5.tile should equal(expectedTileIndexes(1))

    val dependency75 = indexMap.get(75).get
    dependency75.tile should equal(expectedTileIndexes(7))
  }
}
