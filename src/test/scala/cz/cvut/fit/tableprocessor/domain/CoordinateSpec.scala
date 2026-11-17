package cz.cvut.fit.tableprocessor.domain

import org.scalatest.funsuite.AnyFunSuite

class CoordinateSpec extends AnyFunSuite {
  test("renders spreadsheet references, including multi-letter columns") {
    assert(Coordinate(0, 0).reference == "A1")
    assert(Coordinate(25, 4).reference == "Z5")
    assert(Coordinate(26, 11).reference == "AA12")
    assert(Coordinate(701, 0).reference == "ZZ1")
  }

  test("parses case-insensitive spreadsheet references") {
    assert(Coordinate.fromReference("a1") == Right(Coordinate(0, 0)))
    assert(Coordinate.fromReference(" AA12 ") == Right(Coordinate(26, 11)))
  }

  test("parses spreadsheet column names") {
    assert(Coordinate.fromColumnName("A") == Right(0))
    assert(Coordinate.fromColumnName("b") == Right(1))
    assert(Coordinate.fromColumnName("Z") == Right(25))
    assert(Coordinate.fromColumnName(" AA ") == Right(26))
  }

  test("rejects invalid spreadsheet column names") {
    assert(Coordinate.fromColumnName("").isLeft)
    assert(Coordinate.fromColumnName("A1").isLeft)
    assert(Coordinate.fromColumnName("A-").isLeft)
  }

  test("rejects malformed spreadsheet references") {
    assert(Coordinate.fromReference("A0").isLeft)
    assert(Coordinate.fromReference("1A").isLeft)
    assert(Coordinate.fromReference("A-1").isLeft)
    assert(Coordinate.fromReference("A").isLeft)
  }
}
