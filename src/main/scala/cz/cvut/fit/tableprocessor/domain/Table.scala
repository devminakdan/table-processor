package cz.cvut.fit.tableprocessor.domain

// Immutable rectangular table.
final case class Table private (rows: Vector[TableRow]) {
  val height: Int = rows.length
  val width: Int = rows.headOption.fold(0)(_.cells.length)

  def cellAt(coordinate: Coordinate): Option[Cell] =
    rows.find(_.originalIndex == coordinate.row).flatMap(_.cells.lift(coordinate.column))

  def contains(coordinate: Coordinate): Boolean = cellAt(coordinate).isDefined

  def coordinates: Iterator[Coordinate] =
    for {
      row <- rows.iterator
      column <- Iterator.range(0, width)
    } yield Coordinate(column, row.originalIndex)

  def mapCells(transform: (Coordinate, Cell) => Cell): Table =
    Table.unsafeFromTableRows(
      rows.map { row =>
        val transformedCells = row.cells.zipWithIndex.map { case (cell, columnIndex) =>
          transform(Coordinate(columnIndex, row.originalIndex), cell)
        }

        row.copy(cells = transformedCells)
      }
    )
}

object Table {
  def fromRows(rows: Seq[Seq[Cell]]): Either[String, Table] = {
    val tableRows = rows.zipWithIndex.map { case (cells, index) =>
      TableRow(index, cells.toVector)
    }

    fromTableRows(tableRows)
  }

  def fromTableRows(rows: Seq[TableRow]): Either[String, Table] = {
    val normalizedRows = rows.toVector
    val expectedWidth = normalizedRows.headOption.fold(0)(_.cells.length)

    if (normalizedRows.exists(_.cells.length != expectedWidth)) {
      Left("A table must be rectangular: every row must have the same number of cells")
    } else if (normalizedRows.map(_.originalIndex).distinct.length != normalizedRows.length) {
      Left("A table cannot contain duplicate original row indexes")
    } else {
      Right(new Table(normalizedRows))
    }
  }

  def empty: Table = new Table(Vector.empty)

  private[domain] def unsafeFromTableRows(rows: Vector[TableRow]): Table =
    new Table(rows)
}
