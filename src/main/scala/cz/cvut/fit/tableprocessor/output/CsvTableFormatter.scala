package cz.cvut.fit.tableprocessor.output

import cz.cvut.fit.tableprocessor.domain.Cell
import cz.cvut.fit.tableprocessor.domain.Table

final class CsvTableFormatter {
  // Converts a table into delimiter-separated text.
  def format(table: Table, separator: String): String = {
    require(separator.nonEmpty, "Separator cannot be empty")

    val lines = table.rows.map { row =>
      row.cells.map(formatCell).mkString(separator)
    }

    lines.mkString("\n")
  }

  // Converts one table cell into its output text.
  private def formatCell(cell: Cell): String = {
    cell match {
      case Cell.Empty => ""
      case Cell.Number(value) => value.toString
      case Cell.Formula(source) => source
    }
  }
}
