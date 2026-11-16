package cz.cvut.fit.tableprocessor.domain

// Immutable rectangular table.
final case class Table private (rows: Vector[Vector[Cell]]) {
  val height: Int = rows.length
  val width: Int = rows.headOption.fold(0)(_.length)

  def cellAt(coordinate: Coordinate): Option[Cell] =
    rows.lift(coordinate.row).flatMap(_.lift(coordinate.column))

  def contains(coordinate: Coordinate): Boolean = cellAt(coordinate).isDefined

  def coordinates: Iterator[Coordinate] =
    for {
      row <- Iterator.range(0, height)
      column <- Iterator.range(0, width)
    } yield Coordinate(column, row)

  def mapCells(transform: (Coordinate, Cell) => Cell): Table =
    Table.unsafeFromRows(
      rows.zipWithIndex.map { case (row, rowIndex) =>
        row.zipWithIndex.map { case (cell, columnIndex) =>
          transform(Coordinate(columnIndex, rowIndex), cell)
        }
      }
    )
}

object Table {
  def fromRows(rows: Seq[Seq[Cell]]): Either[String, Table] = {
    val normalizedRows = rows.map(_.toVector).toVector
    val expectedWidth = normalizedRows.headOption.fold(0)(_.length)

    if (normalizedRows.exists(_.length != expectedWidth)) {
      Left("A table must be rectangular: every row must have the same number of cells")
    } else {
      Right(new Table(normalizedRows))
    }
  }

  def empty: Table = new Table(Vector.empty)

  private[domain] def unsafeFromRows(rows: Vector[Vector[Cell]]): Table =
    new Table(rows)
}
