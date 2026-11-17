package cz.cvut.fit.tableprocessor.cli

import org.scalatest.funsuite.AnyFunSuite

class CommandLineOptionsSpec extends AnyFunSuite {
  test("uses comma as the default separator") {
    val options = parse(Vector("--input-file", "input.csv"))

    assert(options.inputFile == "input.csv")
    assert(options.separator == ",")
    assert(options.filters.isEmpty)
  }

  test("parses an input separator between filter commands") {
    val options = parse(Vector(
      "--input-file", "input.csv",
      "--filter", "A", ">", "2",
      "--input-separator", ";",
      "--filter-empty", "B"
    ))

    assert(options.separator == ";")
    assert(options.filters.length == 2)
  }

  test("rejects a missing or empty separator") {
    assert(CommandLineOptions.parse(Vector("--input-file", "input.csv", "--input-separator")) ==
      Left("--input-separator requires a value"))
    assert(CommandLineOptions.parse(Vector("--input-file", "input.csv", "--input-separator", "")) ==
      Left("Separator cannot be empty"))
  }

  private def parse(arguments: Vector[String]): CommandLineOptions = {
    CommandLineOptions.parse(arguments) match {
      case Right(options) => options
      case Left(error) => fail(s"Expected valid command-line options, but got: $error")
    }
  }
}
