package cz.cvut.fit.tableprocessor.filtering

import cz.cvut.fit.tableprocessor.domain.Cell
import cz.cvut.fit.tableprocessor.domain.Table
import cz.cvut.fit.tableprocessor.domain.TableRow
import scala.collection.mutable.ArrayBuffer

final class ComparisonFilter(
    column: Int,
    operator: ComparisonOperator,
    target: BigInt
) extends TableFilter {
  require(column >= 0, "Column index cannot be negative")

  // Keeps rows with a matching numeric value in one column.
  override def apply(table: Table): Table = {
    val acceptedRows = ArrayBuffer.empty[TableRow]
    var rowPosition = 0

    while (rowPosition < table.rows.length) {
      val row = table.rows(rowPosition)

      row.cells.lift(column) match {
        case Some(Cell.Number(value)) if operator.matches(value, target) =>
          acceptedRows.addOne(row)
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
