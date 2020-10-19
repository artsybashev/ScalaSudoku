package sudoku

import org.apache.commons.lang3.StringUtils

class SudokuViewProvider(val ruleProvider: RuleProviderInterface) {
  private val cellLength = ruleProvider.maxValue.toString.length
  private val dividerCellLine = StringUtils.repeat('═', cellLength)
  private val rank = ruleProvider.rank
  private val header = makeDivider('╔', '╗', '╤', '╦')
  private val footer = makeDivider('╚', '╝', '╧', '╩')
  private val divider = makeDivider('╠', '╣', '╪', '╬')
  private val space = StringUtils.repeat(' ', cellLength)

  private def makeDivider(leftCorner: Char, rightCorner: Char, cellDivider: Char, tileDivider: Char): String = {
    val sb = new StringBuilder()
    sb.append(leftCorner)
    for (j <- 0 until ruleProvider.sideLength) {
      sb.append(dividerCellLine)
      if (j < ruleProvider.sideLength - 1) {
        sb.append(if ((j + 1) % rank == 0 && j > 0) tileDivider else cellDivider)
      }
    }
    sb.append(rightCorner)
    sb.append("\n")
    sb.toString
  }

  def mkString(sudokuField: SudokuField): String = {
    val shift = ruleProvider.sideLength

    def needDivider(hPosition: Int): Boolean = {
      (hPosition + shift) % (rank * shift) == 0 && hPosition > 0 && hPosition < ruleProvider.size - shift

    }

    val sb = new StringBuilder()
    sb.append(header)
    for (i <- 0 until ruleProvider.size by ruleProvider.sideLength) {
      for (j <- 0 until ruleProvider.sideLength) {
        sb.append(if (j % rank == 0) '║' else '│')
        val c = sudokuField.cells(i + j)
        if (c == 0) sb.append(space)
        else sb.append(StringUtils.leftPad(c.toString, cellLength))
      }
      sb.append("║\n")
      if (needDivider(i)) sb.append(divider)
    }
    sb.append(footer)
    sb.toString
  }
}
