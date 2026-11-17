package cz.cvut.fit.tableprocessor.domain

final case class TableRow(originalIndex: Int, cells: Vector[Cell]) {
  require(originalIndex >= 0, "Original row index cannot be negative")
}
