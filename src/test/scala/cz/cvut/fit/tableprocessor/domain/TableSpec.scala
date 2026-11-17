package cz.cvut.fit.tableprocessor.domain

import org.scalatest.funsuite.AnyFunSuite

class TableSpec extends AnyFunSuite {
  test("keeps dimensions and gives access to cells by coordinates") {
    val table = Table.fromRows(Seq(
      Seq(Cell.Number(1), Cell.Empty),
      Seq(Cell.Formula("=A1"), Cell.Number(4))
    )).toOption.get

    assert(table.width == 2)
    assert(table.height == 2)
    assert(table.cellAt(Coordinate(0, 1)).contains(Cell.Formula("=A1")))
    assert(table.cellAt(Coordinate(2, 0)).isEmpty)
  }

  test("rejects non-rectangular input") {
    val result = Table.fromRows(Seq(
      Seq(Cell.Number(1)),
      Seq(Cell.Number(2), Cell.Number(3))
    ))

    assert(result.isLeft)
  }

  test("maps cells without mutating the original table") {
    val original = Table.fromRows(Seq(Seq(Cell.Number(1)))).toOption.get
    val changed = original.mapCells((_, _) => Cell.Number(2))

    assert(original.cellAt(Coordinate(0, 0)).contains(Cell.Number(1)))
    assert(changed.cellAt(Coordinate(0, 0)).contains(Cell.Number(2)))
  }

  test("keeps original row coordinates after rows are removed") {
    val table = Table.fromTableRows(Seq(
      TableRow(0, Vector(Cell.Number(10))),
      TableRow(2, Vector(Cell.Number(30)))
    )).toOption.get

    assert(table.height == 2)
    assert(table.cellAt(Coordinate(0, 0)).contains(Cell.Number(10)))
    assert(table.cellAt(Coordinate(0, 1)).isEmpty)
    assert(table.cellAt(Coordinate(0, 2)).contains(Cell.Number(30)))
    assert(table.coordinates.toVector == Vector(Coordinate(0, 0), Coordinate(0, 2)))
  }

  test("rejects duplicate original row indexes") {
    val result = Table.fromTableRows(Seq(
      TableRow(0, Vector(Cell.Number(10))),
      TableRow(0, Vector(Cell.Number(20)))
    ))

    assert(result.isLeft)
  }
}
