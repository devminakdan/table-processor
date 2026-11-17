package cz.cvut.fit.tableprocessor.output

import cz.cvut.fit.tableprocessor.domain.Cell
import cz.cvut.fit.tableprocessor.domain.Table
import org.scalatest.funsuite.AnyFunSuite

class CsvTableFormatterSpec extends AnyFunSuite {
  private val formatter = new CsvTableFormatter

  test("formats numbers and empty cells as CSV") {
    val table = createTable(Seq(
      Seq(Cell.Number(12), Cell.Empty, Cell.Number(-3)),
      Seq(Cell.Empty, Cell.Number(7), Cell.Empty)
    ))

    assert(formatter.format(table, ",") == "12,,-3\n,7,")
  }

  test("uses the requested separator") {
    val table = createTable(Seq(Seq(Cell.Number(1), Cell.Number(2))))

    assert(formatter.format(table, ";") == "1;2")
  }

  test("formats an empty table as empty text") {
    assert(formatter.format(Table.empty, ",") == "")
  }

  private def createTable(rows: Seq[Seq[Cell]]): Table = {
    Table.fromRows(rows) match {
      case Right(table) => table
      case Left(error) => fail(s"Expected a valid test table, but got: $error")
    }
  }
}
