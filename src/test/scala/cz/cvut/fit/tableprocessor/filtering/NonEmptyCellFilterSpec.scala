package cz.cvut.fit.tableprocessor.filtering

import cz.cvut.fit.tableprocessor.domain.Cell
import cz.cvut.fit.tableprocessor.domain.Table
import org.scalatest.funsuite.AnyFunSuite

class NonEmptyCellFilterSpec extends AnyFunSuite {
  test("keeps rows with a numeric cell in the selected column") {
    val input = createTable(Seq(
      Seq(Cell.Number(10), Cell.Empty),
      Seq(Cell.Number(20), Cell.Number(5)),
      Seq(Cell.Number(30), Cell.Number(7))
    ))
    val filter = new NonEmptyCellFilter(1)

    val result = filter(input)

    assert(result.rows.map(_.originalIndex) == Vector(1, 2))
  }

  test("chains with a comparison filter and preserves original row indexes") {
    val input = createTable(Seq(
      Seq(Cell.Number(10), Cell.Empty),
      Seq(Cell.Number(20), Cell.Number(5)),
      Seq(Cell.Number(30), Cell.Number(7))
    ))
    val nonEmptyInSecondColumn = new NonEmptyCellFilter(1)
    val greaterThanTwenty = new ComparisonFilter(0, ComparisonOperator.GreaterThan, 20)

    val result = greaterThanTwenty(nonEmptyInSecondColumn(input))

    assert(result.rows.map(_.originalIndex) == Vector(2))
  }

  private def createTable(rows: Seq[Seq[Cell]]): Table = {
    Table.fromRows(rows) match {
      case Right(table) => table
      case Left(error) => fail(s"Expected a valid test table, but got: $error")
    }
  }
}
