package cz.cvut.fit.tableprocessor.filtering

import cz.cvut.fit.tableprocessor.domain.Cell
import cz.cvut.fit.tableprocessor.domain.Table
import org.scalatest.funsuite.AnyFunSuite

class ComparisonFilterSpec extends AnyFunSuite {
  test("supports all numeric comparison operators") {
    val input = createTable(Seq(
      Seq(Cell.Number(5)),
      Seq(Cell.Number(10)),
      Seq(Cell.Number(15)),
      Seq(Cell.Empty)
    ))

    assert(originalIndexes(input, ComparisonOperator.LessThan, 10) == Vector(0))
    assert(originalIndexes(input, ComparisonOperator.LessThanOrEqual, 10) == Vector(0, 1))
    assert(originalIndexes(input, ComparisonOperator.GreaterThan, 10) == Vector(2))
    assert(originalIndexes(input, ComparisonOperator.GreaterThanOrEqual, 10) == Vector(1, 2))
    assert(originalIndexes(input, ComparisonOperator.Equal, 10) == Vector(1))
    assert(originalIndexes(input, ComparisonOperator.NotEqual, 10) == Vector(0, 2))
  }

  test("does not keep empty cells in numeric filters") {
    val input = createTable(Seq(
      Seq(Cell.Empty),
      Seq(Cell.Number(10))
    ))
    val filter = new ComparisonFilter(0, ComparisonOperator.NotEqual, 10)

    assert(filter(input).rows.isEmpty)
  }

  test("chains filters and preserves original row indexes") {
    val input = createTable(Seq(
      Seq(Cell.Number(5)),
      Seq(Cell.Number(10)),
      Seq(Cell.Number(15)),
      Seq(Cell.Number(20))
    ))
    val greaterThanFive = new ComparisonFilter(0, ComparisonOperator.GreaterThan, 5)
    val lessThanTwenty = new ComparisonFilter(0, ComparisonOperator.LessThan, 20)

    val result = lessThanTwenty(greaterThanFive(input))

    assert(result.rows.map(_.originalIndex) == Vector(1, 2))
  }

  private def originalIndexes(
      table: Table,
      operator: ComparisonOperator,
      target: BigInt
  ): Vector[Int] = {
    val filter = new ComparisonFilter(0, operator, target)
    filter(table).rows.map(_.originalIndex)
  }

  private def createTable(rows: Seq[Seq[Cell]]): Table = {
    Table.fromRows(rows) match {
      case Right(table) => table
      case Left(error) => fail(s"Expected a valid test table, but got: $error")
    }
  }
}
