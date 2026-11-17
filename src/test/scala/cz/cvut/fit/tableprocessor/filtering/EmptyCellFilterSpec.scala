package cz.cvut.fit.tableprocessor.filtering

import cz.cvut.fit.tableprocessor.domain.Cell
import cz.cvut.fit.tableprocessor.domain.Table
import org.scalatest.funsuite.AnyFunSuite

class EmptyCellFilterSpec extends AnyFunSuite {
  test("keeps rows with an empty cell in the selected column") {
    val input = createTable(Seq(
      Seq(Cell.Number(10), Cell.Empty),
      Seq(Cell.Number(20), Cell.Number(5)),
      Seq(Cell.Number(30), Cell.Empty)
    ))
    val filter = new EmptyCellFilter(1)

    val result = filter(input)

    assert(result.rows.map(_.originalIndex) == Vector(0, 2))
  }

  test("chains with another filter and preserves original row indexes") {
    val input = createTable(Seq(
      Seq(Cell.Number(10), Cell.Empty),
      Seq(Cell.Number(20), Cell.Empty),
      Seq(Cell.Number(30), Cell.Number(5))
    ))
    val greaterThanFifteen = new ComparisonFilter(0, ComparisonOperator.GreaterThan, 15)
    val emptyInSecondColumn = new EmptyCellFilter(1)

    val result = emptyInSecondColumn(greaterThanFifteen(input))

    assert(result.rows.map(_.originalIndex) == Vector(1))
  }

  private def createTable(rows: Seq[Seq[Cell]]): Table = {
    Table.fromRows(rows) match {
      case Right(table) => table
      case Left(error) => fail(s"Expected a valid test table, but got: $error")
    }
  }
}
