package cz.cvut.fit.tableprocessor.formula

import org.scalatest.funsuite.AnyFunSuite

class BinaryOperatorSpec extends AnyFunSuite {
  test("evaluates addition") {
    assert(BinaryOperator.Addition.evaluate(10, 5) == Right(15))
  }

  test("evaluates subtraction") {
    assert(BinaryOperator.Subtraction.evaluate(10, 15) == Right(-5))
  }

  test("evaluates multiplication") {
    assert(BinaryOperator.Multiplication.evaluate(10, 5) == Right(50))
  }

  test("evaluates integer division") {
    assert(BinaryOperator.Division.evaluate(11, 2) == Right(5))
  }

  test("reports division by zero") {
    assert(BinaryOperator.Division.evaluate(10, 0) == Left("Division by zero"))
  }

  test("assigns higher precedence to multiplication and division") {
    assert(BinaryOperator.Addition.precedence == 1)
    assert(BinaryOperator.Subtraction.precedence == 1)
    assert(BinaryOperator.Multiplication.precedence == 2)
    assert(BinaryOperator.Division.precedence == 2)
  }

  test("contains all standard operators") {
    val operators = BinaryOperator.standard

    assert(operators.contains(BinaryOperator.Addition))
    assert(operators.contains(BinaryOperator.Subtraction))
    assert(operators.contains(BinaryOperator.Multiplication))
    assert(operators.contains(BinaryOperator.Division))
  }
}
