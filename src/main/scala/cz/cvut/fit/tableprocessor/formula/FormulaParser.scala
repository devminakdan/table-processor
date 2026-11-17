package cz.cvut.fit.tableprocessor.formula

import cz.cvut.fit.tableprocessor.formula.Expression.BinaryExpression
import cz.cvut.fit.tableprocessor.formula.Expression.CellReference
import cz.cvut.fit.tableprocessor.formula.Expression.NumberLiteral
import scala.collection.mutable.ArrayBuffer

final class FormulaParser {
  // Builds an abstract syntax tree (AST) from formula tokens.
  def parse(tokens: Vector[FormulaToken]): Either[String, Expression] = {
    if (tokens.isEmpty) {
      return Left("Formula cannot be empty")
    }

    val expressions = ArrayBuffer.empty[Expression]
    val operators = ArrayBuffer.empty[BinaryOperator]
    var index = 0
    var expectingOperand = true

    while (index < tokens.length) {
      val token = tokens(index)

      if (expectingOperand) {
        token match {
          case FormulaToken.Number(value) =>
            expressions.addOne(NumberLiteral(value))
            expectingOperand = false
          case FormulaToken.Reference(coordinate) =>
            expressions.addOne(CellReference(coordinate))
            expectingOperand = false
          case FormulaToken.Operator(_) =>
            return Left(s"Expected a number or cell reference at token ${index + 1}")
        }
      } else {
        token match {
          case FormulaToken.Operator(operator) =>
            while (
              operators.length > 0 &&
                operators(operators.length - 1).precedence >= operator.precedence
            ) {
              combineLastOperation(expressions, operators)
            }

            operators.addOne(operator)
            expectingOperand = true
          case _ => return Left(s"Expected an operator at token ${index + 1}")
        }
      }

      index += 1
    }

    if (expectingOperand) {
      return Left("Formula cannot end with an operator")
    }

    while (operators.length > 0) {
      combineLastOperation(expressions, operators)
    }

    if (expressions.length == 1) {
      Right(expressions(0))
    } else {
      Left("Invalid formula")
    }
  }

  // Combines the last operator with its two latest expressions.
  private def combineLastOperation(
      expressions: ArrayBuffer[Expression],
      operators: ArrayBuffer[BinaryOperator]
  ): Unit = {
    val operator = operators.remove(operators.length - 1)
    val right = expressions.remove(expressions.length - 1)
    val left = expressions.remove(expressions.length - 1)
    expressions.addOne(BinaryExpression(left, operator, right))
  }
}
