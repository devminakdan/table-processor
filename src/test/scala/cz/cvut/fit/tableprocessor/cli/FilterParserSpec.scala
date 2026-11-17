package cz.cvut.fit.tableprocessor.cli

import cz.cvut.fit.tableprocessor.domain.Cell
import cz.cvut.fit.tableprocessor.domain.Table
import cz.cvut.fit.tableprocessor.filtering.TableFilter
import org.scalatest.funsuite.AnyFunSuite

class FilterParserSpec extends AnyFunSuite {
  test("parses and applies comparison, empty and non-empty filters") {
    val filters = parseFilters(Vector(
      "--filter", "A", ">", "5",
      "--filter-empty", "B",
      "--filter-non-empty", "C"
    ))
    val input = createTable(Seq(
      Seq(Cell.Number(10), Cell.Empty, Cell.Number(1)),
      Seq(Cell.Number(10), Cell.Empty, Cell.Empty),
      Seq(Cell.Number(5), Cell.Empty, Cell.Number(1)),
      Seq(Cell.Number(10), Cell.Number(2), Cell.Number(1))
    ))

    assert(applyFilters(input, filters).rows.map(_.originalIndex) == Vector(0))
  }

  test("parses multi-letter column names") {
    val cells = Vector.fill(27)(Cell.Empty).updated(26, Cell.Number(8))
    val input = createTable(Seq(cells))
    val filters = parseFilters(Vector("--filter", "AA", ">=", "8"))

    assert(applyFilters(input, filters).rows.map(_.originalIndex) == Vector(0))
  }

  test("rejects incomplete or invalid filter arguments") {
    assert(FilterParser.parse(Vector("--filter", "A", ">")) ==
      Left("--filter requires a column, an operator and a number"))
    assert(FilterParser.parse(Vector("--filter-empty")) ==
      Left("--filter-empty requires a column"))
    assert(FilterParser.parse(Vector("--filter", "A", "~", "10")).isLeft)
    assert(FilterParser.parse(Vector("--filter", "A1", ">", "10")).isLeft)
    assert(FilterParser.parse(Vector("--filter", "A", ">", "ten")).isLeft)
    assert(FilterParser.parse(Vector("--other")).isLeft)
  }

  private def parseFilters(arguments: Vector[String]): Vector[TableFilter] = {
    FilterParser.parse(arguments) match {
      case Right(filters) => filters
      case Left(error) => fail(s"Expected valid filter arguments, but got: $error")
    }
  }

  private def applyFilters(table: Table, filters: Vector[TableFilter]): Table = {
    var filteredTable = table
    var index = 0

    while (index < filters.length) {
      filteredTable = filters(index)(filteredTable)
      index += 1
    }

    filteredTable
  }

  private def createTable(rows: Seq[Seq[Cell]]): Table = {
    Table.fromRows(rows) match {
      case Right(table) => table
      case Left(error) => fail(s"Expected a valid test table, but got: $error")
    }
  }
}
