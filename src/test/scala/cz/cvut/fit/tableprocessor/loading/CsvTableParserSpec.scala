package cz.cvut.fit.tableprocessor.loading

import cz.cvut.fit.tableprocessor.domain.Cell
import cz.cvut.fit.tableprocessor.domain.Coordinate
import org.scalatest.funsuite.AnyFunSuite

class CsvTableParserSpec extends AnyFunSuite {
  private val parser = new CsvTableParser(new CellParser)

  test("parses a comma-separated table") {
    parser.parse("1,2,=A1+B1\n3,,4", ",") match {
      case Right(table) =>
        assert(table.height == 2)
        assert(table.width == 3)
        assert(table.cellAt(Coordinate(0, 0)).contains(Cell.Number(1)))
        assert(table.cellAt(Coordinate(2, 0)).contains(Cell.Formula("=A1+B1")))
        assert(table.cellAt(Coordinate(1, 1)).contains(Cell.Empty))
      case Left(error) => fail(s"Expected a parsed table, but got: $error")
    }
  }

  test("supports another separator and trailing empty cells") {
    parser.parse("1;;\n2;3;", ";") match {
      case Right(table) =>
        assert(table.height == 2)
        assert(table.width == 3)
        assert(table.cellAt(Coordinate(1, 0)).contains(Cell.Empty))
        assert(table.cellAt(Coordinate(2, 1)).contains(Cell.Empty))
      case Left(error) => fail(s"Expected a parsed table, but got: $error")
    }
  }

  test("rejects rows with different widths") {
    assert(parser.parse("1,2\n3", ",").isLeft)
  }

  test("reports the coordinate of an invalid cell") {
    val result = parser.parse("1,2\n3,text", ",")

    assert(result == Left("Invalid cell at B2: Invalid cell value: 'text'"))
  }

  test("rejects an empty separator") {
    assert(parser.parse("1,2", "").isLeft)
  }
}
