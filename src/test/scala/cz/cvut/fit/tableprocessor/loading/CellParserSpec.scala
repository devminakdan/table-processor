package cz.cvut.fit.tableprocessor.loading

import cz.cvut.fit.tableprocessor.domain.Cell
import org.scalatest.funsuite.AnyFunSuite

class CellParserSpec extends AnyFunSuite {
  private val parser = new CellParser

  test("parses empty cells") {
    assert(parser.parse("") == Right(Cell.Empty))
    assert(parser.parse("   ") == Right(Cell.Empty))
  }

  test("parses numbers and formulas") {
    assert(parser.parse("0") == Right(Cell.Number(0)))
    assert(parser.parse(" 42 ") == Right(Cell.Number(42)))
    assert(parser.parse(" =A1 + B2 ") == Right(Cell.Formula("=A1 + B2")))
  }

  test("rejects unsupported cell values") {
    assert(parser.parse("-1").isLeft)
    assert(parser.parse("text").isLeft)
    assert(parser.parse("1.5").isLeft)
  }
}
