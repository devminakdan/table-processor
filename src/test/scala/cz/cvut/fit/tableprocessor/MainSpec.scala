package cz.cvut.fit.tableprocessor

import java.nio.file.Files
import org.scalatest.funsuite.AnyFunSuite

class MainSpec extends AnyFunSuite {
  test("loads, evaluates, filters and formats a CSV file") {
    val inputFile = Files.createTempFile("table-processor-", ".csv")
    Files.writeString(inputFile, "3,=A1*2,7\n10,=A2-15,2")

    try {
      val result = Main.process(Vector(
        "--input-file",
        inputFile.toString,
        "--filter", "B", ">", "0",
        "--filter-non-empty", "C"
      ))

      assert(result == Right("3,6,7"))
    } finally {
      Files.deleteIfExists(inputFile)
    }
  }

  test("returns a usage error when no input file is provided") {
    assert(Main.process(Vector.empty) == Left("Usage: table-processor --input-file <input.csv> [options]"))
  }

  test("requires --input-file before the file name") {
    assert(Main.process(Vector("input.csv")).isLeft)
  }

  test("uses the requested separator for input and output") {
    val inputFile = Files.createTempFile("table-processor-", ".csv")
    Files.writeString(inputFile, "3;=A1*2;7")

    try {
      val result = Main.process(Vector(
        "--input-file", inputFile.toString,
        "--input-separator", ";",
        "--filter", "B", ">", "0"
      ))

      assert(result == Right("3;6;7"))
    } finally {
      Files.deleteIfExists(inputFile)
    }
  }
}
