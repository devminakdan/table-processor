package cz.cvut.fit.tableprocessor.formula

import cz.cvut.fit.tableprocessor.domain.Coordinate
import cz.cvut.fit.tableprocessor.formula.EvaluatedValue.Empty
import cz.cvut.fit.tableprocessor.formula.EvaluatedValue.Number
import cz.cvut.fit.tableprocessor.formula.Expression.BinaryExpression
import cz.cvut.fit.tableprocessor.formula.Expression.CellReference
import cz.cvut.fit.tableprocessor.formula.Expression.NumberLiteral
import org.scalatest.funsuite.AnyFunSuite

class FormulaEvaluatorSpec extends AnyFunSuite {
  private val evaluator = new FormulaEvaluator

  test("evaluates nested binary expressions") {
    val expression = BinaryExpression(
      NumberLiteral(2),
      BinaryOperator.Addition,
      BinaryExpression(
        NumberLiteral(3),
        BinaryOperator.Multiplication,
        NumberLiteral(4)
      )
    )

    val result = evaluator.evaluate(expression, missingReference)

    assert(result == Right(Number(14)))
  }

  test("uses the reference resolver") {
    val expression = BinaryExpression(
      CellReference(Coordinate(0, 0)),
      BinaryOperator.Addition,
      NumberLiteral(2)
    )

    val result = evaluator.evaluate(expression, coordinate => {
      if (coordinate == Coordinate(0, 0)) {
        Right(Number(10))
      } else {
        Left("Unknown cell")
      }
    })

    assert(result == Right(Number(12)))
  }

  test("returns empty for a direct reference to an empty cell") {
    val expression = CellReference(Coordinate(0, 0))

    val result = evaluator.evaluate(expression, _ => Right(Empty))

    assert(result == Right(Empty))
  }

  test("rejects arithmetic with an empty cell") {
    val expression = BinaryExpression(
      CellReference(Coordinate(0, 0)),
      BinaryOperator.Addition,
      NumberLiteral(2)
    )

    val result = evaluator.evaluate(expression, _ => Right(Empty))

    assert(result.isLeft)
  }

  test("returns errors from an operator") {
    val expression = BinaryExpression(
      NumberLiteral(10),
      BinaryOperator.Division,
      NumberLiteral(0)
    )

    assert(evaluator.evaluate(expression, missingReference) == Left("Division by zero"))
  }

  private def missingReference(coordinate: Coordinate): Either[String, EvaluatedValue] =
    Left(s"Unknown cell: ${coordinate.reference}")
}
