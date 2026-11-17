package cz.cvut.fit.tableprocessor.formula

import cz.cvut.fit.tableprocessor.domain.Coordinate
import cz.cvut.fit.tableprocessor.formula.EvaluatedValue.Empty
import cz.cvut.fit.tableprocessor.formula.EvaluatedValue.Number
import cz.cvut.fit.tableprocessor.formula.Expression.BinaryExpression
import cz.cvut.fit.tableprocessor.formula.Expression.CellReference
import cz.cvut.fit.tableprocessor.formula.Expression.NumberLiteral

final class FormulaEvaluator {
  // Evaluates an expression using the provided cell reference resolver.
  def evaluate(
      expression: Expression,
      resolveReference: Coordinate => Either[String, EvaluatedValue]
  ): Either[String, EvaluatedValue] = {
    expression match {
      case NumberLiteral(value) => Right(Number(value))
      case CellReference(coordinate) => resolveReference(coordinate)
      case BinaryExpression(left, operator, right) =>
        evaluate(left, resolveReference) match {
          case Left(error) => Left(error)
          case Right(leftValue) =>
            evaluate(right, resolveReference) match {
              case Left(error) => Left(error)
              case Right(rightValue) => evaluateOperation(leftValue, operator, rightValue)
            }
        }
    }
  }

  // Applies an operator to two evaluated values.
  private def evaluateOperation(
      left: EvaluatedValue,
      operator: BinaryOperator,
      right: EvaluatedValue
  ): Either[String, EvaluatedValue] = {
    (left, right) match {
      case (Number(leftNumber), Number(rightNumber)) =>
        operator.evaluate(leftNumber, rightNumber) match {
          case Right(result) => Right(Number(result))
          case Left(error) => Left(error)
        }
      case _ => Left(s"Cannot use an empty cell with operator '${operator.symbol}'")
    }
  }
}
