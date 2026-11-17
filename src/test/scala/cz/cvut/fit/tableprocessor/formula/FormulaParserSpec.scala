package cz.cvut.fit.tableprocessor.formula

import cz.cvut.fit.tableprocessor.domain.Coordinate
import cz.cvut.fit.tableprocessor.formula.Expression.BinaryExpression
import cz.cvut.fit.tableprocessor.formula.Expression.CellReference
import cz.cvut.fit.tableprocessor.formula.Expression.NumberLiteral
import org.scalatest.funsuite.AnyFunSuite

class FormulaParserSpec extends AnyFunSuite {
  private val parser = new FormulaParser

  test("builds an AST with operator precedence") {
    val tokens = Vector(
      FormulaToken.Number(2),
      FormulaToken.Operator(BinaryOperator.Addition),
      FormulaToken.Number(3),
      FormulaToken.Operator(BinaryOperator.Multiplication),
      FormulaToken.Number(4)
    )
    val expected = BinaryExpression(
      NumberLiteral(2),
      BinaryOperator.Addition,
      BinaryExpression(
        NumberLiteral(3),
        BinaryOperator.Multiplication,
        NumberLiteral(4)
      )
    )

    assert(parser.parse(tokens) == Right(expected))
  }

  test("keeps operators with the same precedence left-associative") {
    val tokens = Vector(
      FormulaToken.Number(10),
      FormulaToken.Operator(BinaryOperator.Subtraction),
      FormulaToken.Number(2),
      FormulaToken.Operator(BinaryOperator.Subtraction),
      FormulaToken.Number(3)
    )
    val expected = BinaryExpression(
      BinaryExpression(
        NumberLiteral(10),
        BinaryOperator.Subtraction,
        NumberLiteral(2)
      ),
      BinaryOperator.Subtraction,
      NumberLiteral(3)
    )

    assert(parser.parse(tokens) == Right(expected))
  }

  test("builds an AST with cell references") {
    val tokens = Vector(
      FormulaToken.Reference(Coordinate(0, 0)),
      FormulaToken.Operator(BinaryOperator.Addition),
      FormulaToken.Reference(Coordinate(1, 1))
    )
    val expected = BinaryExpression(
      CellReference(Coordinate(0, 0)),
      BinaryOperator.Addition,
      CellReference(Coordinate(1, 1))
    )

    assert(parser.parse(tokens) == Right(expected))
  }

  test("rejects two consecutive operators") {
    val tokens = Vector(
      FormulaToken.Number(1),
      FormulaToken.Operator(BinaryOperator.Addition),
      FormulaToken.Operator(BinaryOperator.Subtraction),
      FormulaToken.Number(2)
    )

    assert(parser.parse(tokens).isLeft)
  }

  test("rejects a formula ending with an operator") {
    val tokens = Vector(
      FormulaToken.Number(1),
      FormulaToken.Operator(BinaryOperator.Addition)
    )

    assert(parser.parse(tokens).isLeft)
  }
}
