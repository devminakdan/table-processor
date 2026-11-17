package cz.cvut.fit.tableprocessor.formula

import cz.cvut.fit.tableprocessor.domain.Coordinate
import scala.collection.mutable.ArrayBuffer

final class FormulaTokenizer(operators: Vector[BinaryOperator]) {
  // Splits formula text into typed tokens.
  def tokenize(formula: String): Either[String, Vector[FormulaToken]] = {
    if (!formula.startsWith("=")) {
      return Left("Formula must start with '='")
    }

    val tokens = ArrayBuffer.empty[FormulaToken]
    var index = 1

    while (index < formula.length) {
      val current = formula.charAt(index)

      if (Character.isWhitespace(current)) {
        index += 1
      } else if (Character.isDigit(current)) {
        val startIndex = index

        while (index < formula.length && Character.isDigit(formula.charAt(index))) {
          index += 1
        }

        val number = BigInt(formula.substring(startIndex, index))
        tokens += FormulaToken.Number(number)
      } else if (Character.isLetter(current)) {
        val startIndex = index

        while (index < formula.length && Character.isLetter(formula.charAt(index))) {
          index += 1
        }

        while (index < formula.length && Character.isDigit(formula.charAt(index))) {
          index += 1
        }

        val referenceText = formula.substring(startIndex, index)

        Coordinate.fromReference(referenceText) match {
          case Right(coordinate) => tokens += FormulaToken.Reference(coordinate)
          case Left(error) => return Left(error)
        }
      } else {
        findOperator(formula, index) match {
          case Some(operator) =>
            tokens += FormulaToken.Operator(operator)
            index += operator.symbol.length
          case None => return Left(s"Unexpected character '$current' at position ${index + 1}")
        }
      }
    }

    if (tokens.isEmpty) {
      Left("Formula cannot be empty")
    } else {
      Right(tokens.toVector)
    }
  }

  // Finds an operator beginning at the current character position.
  private def findOperator(formula: String, startIndex: Int): Option[BinaryOperator] = {
    var operatorIndex = 0

    while (operatorIndex < operators.length) {
      val operator = operators(operatorIndex)

      if (formula.startsWith(operator.symbol, startIndex)) {
        return Some(operator)
      }

      operatorIndex += 1
    }

    None
  }
}
