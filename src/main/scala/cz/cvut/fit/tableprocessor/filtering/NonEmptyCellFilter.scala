package cz.cvut.fit.tableprocessor.filtering

import cz.cvut.fit.tableprocessor.domain.Cell
import cz.cvut.fit.tableprocessor.domain.Coordinate
import cz.cvut.fit.tableprocessor.domain.Table
import cz.cvut.fit.tableprocessor.domain.TableRow
import scala.collection.mutable.ArrayBuffer

final class NonEmptyCellFilter(column: Int) extends TableFilter {
  require(column >= 0, "Column index cannot be negative")

  // Keeps rows with a non-empty cell in one column.
  override def apply(table: Table): Table = {
    val acceptedRows = ArrayBuffer.empty[TableRow]
    var rowPosition = 0

    while (rowPosition < table.rows.length) {
      val row = table.rows(rowPosition)

      row.cells.lift(column) match {
        case Some(Cell.Number(_)) => acceptedRows.addOne(row)
        case _ =>
      }

      rowPosition += 1
    }

    Table.fromTableRows(acceptedRows.toVector) match {
      case Right(filteredTable) => filteredTable
      case Left(error) => throw new IllegalStateException(error)
    }
  }
}

object NonEmptyCellFilter extends FilterCommandParser {
  override val flag: String = "--filter-non-empty"

  // Parses: <column>.
  override def parse(args: Vector[String]): Either[String, ParseResult] = {
    if (args.isEmpty) {
      Left("--filter-non-empty requires a column")
    } else {
      Coordinate.fromColumnName(args(0)).map { column =>
        ParseResult(new NonEmptyCellFilter(column), consumedArgs = 1)
      }
    }
  }
}
