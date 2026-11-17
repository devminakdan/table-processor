package cz.cvut.fit.tableprocessor.loading

import cz.cvut.fit.tableprocessor.domain.Cell
import cz.cvut.fit.tableprocessor.domain.Coordinate
import java.nio.file.Files
import org.scalatest.funsuite.AnyFunSuite

class FileTableSourceSpec extends AnyFunSuite {
  private val source = new FileTableSource(new CsvTableParser(new CellParser))

  test("loads and parses a table from a file") {
    val file = Files.createTempFile("table-processor-", ".csv")

    try {
      Files.writeString(file, "1,2\n3,=A1+B1")

      source.load(file, ",") match {
        case Right(table) =>
          assert(table.height == 2)
          assert(table.width == 2)
          assert(table.cellAt(Coordinate(0, 1)).contains(Cell.Number(3)))
          assert(table.cellAt(Coordinate(1, 1)).contains(Cell.Formula("=A1+B1")))
        case Left(error) => fail(s"Expected a loaded table, but got: $error")
      }
    } finally {
      Files.deleteIfExists(file)
    }
  }

  test("returns an error when the file does not exist") {
    val missingFile = Files.createTempFile("missing-table-processor-", ".csv")
    Files.deleteIfExists(missingFile)

    assert(source.load(missingFile, ",").isLeft)
  }
}
