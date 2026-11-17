package cz.cvut.fit.tableprocessor.domain

// Coordinates are zero-based: A1 = (0, 0).
final case class Coordinate private (column: Int, row: Int) {
  def reference: String = s"${Coordinate.columnName(column)}${row + 1}"
}

object Coordinate {
  def apply(column: Int, row: Int): Coordinate = {
    require(column >= 0, "Column index cannot be negative")
    require(row >= 0, "Row index cannot be negative")
    new Coordinate(column, row)
  }

  def fromReference(value: String): Either[String, Coordinate] = {
    val reference = value.trim.toUpperCase
    val splitAt = reference.indexWhere(_.isDigit)

    if (splitAt <= 0 || splitAt == reference.length) {
      Left(s"Invalid cell reference: '$value'")
    } else {
      val columnPart = reference.take(splitAt)
      val rowPart = reference.drop(splitAt)

      if (!rowPart.forall(_.isDigit)) {
        Left(s"Invalid cell reference: '$value'")
      } else {
        fromColumnName(columnPart) match {
          case Left(_) => Left(s"Invalid cell reference: '$value'")
          case Right(column) =>
            rowPart.toIntOption match {
              case Some(rowNumber) if rowNumber > 0 => Right(Coordinate(column, rowNumber - 1))
              case _ => Left(s"Invalid row number in cell reference: '$value'")
            }
        }
      }
    }
  }

  // Converts a spreadsheet column name into a zero-based index.
  def fromColumnName(value: String): Either[String, Int] = {
    val columnName = value.trim.toUpperCase

    if (columnName.isEmpty || !columnName.forall(character => character >= 'A' && character <= 'Z')) {
      Left(s"Invalid column name: '$value'")
    } else {
      val index = columnName.foldLeft(0) { (current, character) =>
        current * 26 + (character - 'A' + 1)
      } - 1

      Right(index)
    }
  }

  private def columnName(column: Int): String = {
    require(column >= 0, "Column index cannot be negative")

    Iterator
      .iterate(column + 1)(index => (index - 1) / 26)
      .takeWhile(_ > 0)
      .map(index => ('A' + (index - 1) % 26).toChar)
      .toVector
      .reverse
      .mkString
  }
}
