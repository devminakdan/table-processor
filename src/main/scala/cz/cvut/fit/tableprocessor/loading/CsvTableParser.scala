package cz.cvut.fit.tableprocessor.loading

import cz.cvut.fit.tableprocessor.domain.Cell
import cz.cvut.fit.tableprocessor.domain.Coordinate
import cz.cvut.fit.tableprocessor.domain.Table
import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer

final class CsvTableParser(cellParser: CellParser) {
  def parse(content: String, separator: String): Either[String, Table] = {
    if (separator.isEmpty) {
      Left("Separator cannot be empty")
    } else {
      val parsedRows = ArrayBuffer.empty[Vector[Cell]]
      val lines = content.linesIterator.toVector
      var rowIndex = 0

      while (rowIndex < lines.length) {
        parseRow(lines(rowIndex), separator, rowIndex) match {
          case Right(row) => parsedRows += row
          case Left(error) => return Left(error)
        }
        rowIndex += 1
      }

      Table.fromRows(parsedRows.toVector)
    }
  }

  private def parseRow(
      line: String,
      separator: String,
      rowIndex: Int
  ): Either[String, Vector[Cell]] = {
    val cells = ArrayBuffer.empty[Cell]
    val rawValues = line.split(Pattern.quote(separator), -1)
    var columnIndex = 0

    while (columnIndex < rawValues.length) {
      cellParser.parse(rawValues(columnIndex)) match {
        case Right(cell) => cells += cell
        case Left(error) =>
          val coordinate = Coordinate(columnIndex, rowIndex).reference
          return Left(s"Invalid cell at $coordinate: $error")
      }
      columnIndex += 1
    }

    Right(cells.toVector)
  }
}
