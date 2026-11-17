package cz.cvut.fit.tableprocessor.formula

import cz.cvut.fit.tableprocessor.domain.Cell
import cz.cvut.fit.tableprocessor.domain.Coordinate
import cz.cvut.fit.tableprocessor.domain.Table
import org.scalatest.funsuite.AnyFunSuite

class TableEvaluatorSpec extends AnyFunSuite {
  private val evaluator = new TableEvaluator(
    new FormulaTokenizer(BinaryOperator.standard),
    new FormulaParser,
    new FormulaEvaluator
  )

  test("evaluates a formula using values from other cells") {
    val input = createTable(Seq(
      Seq(Cell.Number(10), Cell.Number(20), Cell.Formula("=A1+B1"))
    ))

    evaluator.evaluate(input) match {
      case Right(table) =>
        assert(table.cellAt(Coordinate(0, 0)).contains(Cell.Number(10)))
        assert(table.cellAt(Coordinate(1, 0)).contains(Cell.Number(20)))
        assert(table.cellAt(Coordinate(2, 0)).contains(Cell.Number(30)))
      case Left(error) => fail(s"Expected an evaluated table, but got: $error")
    }
  }

  test("evaluates formulas that depend on other formulas") {
    val input = createTable(Seq(
      Seq(
        Cell.Number(10),
        Cell.Formula("=A1*2"),
        Cell.Formula("=B1+5")
      )
    ))

    evaluator.evaluate(input) match {
      case Right(table) =>
        assert(table.cellAt(Coordinate(1, 0)).contains(Cell.Number(20)))
        assert(table.cellAt(Coordinate(2, 0)).contains(Cell.Number(25)))
      case Left(error) => fail(s"Expected an evaluated table, but got: $error")
    }
  }

  test("uses operator precedence in a formula") {
    val input = createTable(Seq(Seq(Cell.Formula("=2+3*4"))))

    evaluator.evaluate(input) match {
      case Right(table) =>
        assert(table.cellAt(Coordinate(0, 0)).contains(Cell.Number(14)))
      case Left(error) => fail(s"Expected an evaluated table, but got: $error")
    }
  }

  test("keeps a direct reference to an empty cell empty") {
    val input = createTable(Seq(Seq(Cell.Empty, Cell.Formula("=A1"))))

    evaluator.evaluate(input) match {
      case Right(table) =>
        assert(table.cellAt(Coordinate(1, 0)).contains(Cell.Empty))
      case Left(error) => fail(s"Expected an evaluated table, but got: $error")
    }
  }

  test("rejects arithmetic with an empty cell") {
    val input = createTable(Seq(Seq(Cell.Empty, Cell.Formula("=A1+1"))))

    assert(evaluator.evaluate(input).isLeft)
  }

  test("detects a direct cyclic dependency") {
    val input = createTable(Seq(Seq(Cell.Formula("=A1"))))

    assert(evaluator.evaluate(input) == Left("Cyclic dependency at A1"))
  }

  test("detects an indirect cyclic dependency") {
    val input = createTable(Seq(Seq(Cell.Formula("=B1"), Cell.Formula("=A1"))))

    assert(evaluator.evaluate(input) == Left("Cyclic dependency at A1"))
  }

  test("rejects a reference outside the table") {
    val input = createTable(Seq(Seq(Cell.Formula("=B1"))))

    assert(evaluator.evaluate(input) == Left("Cell reference B1 is outside the table"))
  }

  private def createTable(rows: Seq[Seq[Cell]]): Table = {
    Table.fromRows(rows) match {
      case Right(table) => table
      case Left(error) => fail(s"Expected a valid test table, but got: $error")
    }
  }
}
