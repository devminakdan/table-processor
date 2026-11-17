package cz.cvut.fit.tableprocessor.formula

import cz.cvut.fit.tableprocessor.domain.Cell
import cz.cvut.fit.tableprocessor.domain.Coordinate
import cz.cvut.fit.tableprocessor.domain.Table
import scala.collection.mutable.ArrayBuffer

final class TableEvaluator(
    tokenizer: FormulaTokenizer,
    parser: FormulaParser,
    formulaEvaluator: FormulaEvaluator
) {
  // Evaluates every cell and returns a new table.
  def evaluate(table: Table): Either[String, Table] = {
    val evaluatedRows = ArrayBuffer.empty[Vector[Cell]]
    var rowIndex = 0

    while (rowIndex < table.height) {
      val evaluatedCells = ArrayBuffer.empty[Cell]
      var columnIndex = 0
      while (columnIndex < table.width) {
        val coordinate = Coordinate(columnIndex, rowIndex)

        evaluateCell(table, coordinate, Set.empty) match {
          case Right(value) => evaluatedCells.addOne(toCell(value))
          case Left(error) => return Left(error)
        }
        columnIndex += 1
      }
      evaluatedRows.addOne(evaluatedCells.toVector)
      rowIndex += 1
    }
    Table.fromRows(evaluatedRows.toVector)
  }

  // Evaluates one cell and detects cyclic references.
  private def evaluateCell(table: Table, coordinate: Coordinate, evaluating: Set[Coordinate]): Either[String, EvaluatedValue] = {
    if (evaluating.contains(coordinate)) {
      Left(s"Cyclic dependency at ${coordinate.reference}")
    } else {
      table.cellAt(coordinate) match {
        case None => Left(s"Cell reference ${coordinate.reference} is outside the table")
        case Some(cell) =>
          cell match {
            case Cell.Empty => Right(EvaluatedValue.Empty)
            case Cell.Number(value) => Right(EvaluatedValue.Number(value))
            case Cell.Formula(source) =>
              evaluateFormula(table, coordinate, source, evaluating + coordinate)
          }
      }
    }
  }

  // Tokenizes, parses and evaluates one formula cell.
  private def evaluateFormula(table: Table, coordinate: Coordinate, source: String, evaluating: Set[Coordinate]): Either[String, EvaluatedValue] = {
    tokenizer.tokenize(source) match {
      case Left(error) => Left(s"Invalid formula in ${coordinate.reference}: $error")
      case Right(tokens) =>
        parser.parse(tokens) match {
          case Left(error) => Left(s"Invalid formula in ${coordinate.reference}: $error")
          case Right(expression) =>
            formulaEvaluator.evaluate(
              expression,
              reference => evaluateCell(table, reference, evaluating)
            )
        }
    }
  }

  // Converts an evaluated value back to a table cell.
  private def toCell(value: EvaluatedValue): Cell = {
    value match {
      case EvaluatedValue.Empty => Cell.Empty
      case EvaluatedValue.Number(number) => Cell.Number(number)
    }
  }
}
