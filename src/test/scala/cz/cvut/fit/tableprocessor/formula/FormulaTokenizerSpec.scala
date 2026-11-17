package cz.cvut.fit.tableprocessor.formula

import cz.cvut.fit.tableprocessor.domain.Coordinate
import org.scalatest.funsuite.AnyFunSuite

class FormulaTokenizerSpec extends AnyFunSuite {
  private val tokenizer = new FormulaTokenizer(BinaryOperator.standard)

  test("tokenizes numbers, references and operators without spaces") {
    val result = tokenizer.tokenize("=1+A1*25")
    val expected = Vector(
      FormulaToken.Number(1),
      FormulaToken.Operator(BinaryOperator.Addition),
      FormulaToken.Reference(Coordinate(0, 0)),
      FormulaToken.Operator(BinaryOperator.Multiplication),
      FormulaToken.Number(25)
    )

    assert(result == Right(expected))
  }

  test("ignores whitespace in formulas") {
    val result = tokenizer.tokenize("=  AA12  -  3")
    val expected = Vector(
      FormulaToken.Reference(Coordinate(26, 11)),
      FormulaToken.Operator(BinaryOperator.Subtraction),
      FormulaToken.Number(3)
    )

    assert(result == Right(expected))
  }

  test("rejects a formula without the equals sign") {
    assert(tokenizer.tokenize("A1+2").isLeft)
  }

  test("rejects invalid cell references") {
    assert(tokenizer.tokenize("=A0+2").isLeft)
  }

  test("rejects unexpected characters") {
    assert(tokenizer.tokenize("=A1&2").isLeft)
  }

  test("rejects an empty formula") {
    assert(tokenizer.tokenize("=").isLeft)
  }
}
