package cz.cvut.fit.tableprocessor.cli

import cz.cvut.fit.tableprocessor.filtering.TableFilter
import scala.collection.mutable.ArrayBuffer

final case class CommandLineOptions(
    inputFile: String,
    separator: String,
    filters: Vector[TableFilter]
)

object CommandLineOptions {
  private val defaultSeparator = ","

  // Parses application options and leaves filter details to FilterParser.
  def parse(arguments: Vector[String]): Either[String, CommandLineOptions] = {
    if (arguments.length < 2) {
      Left("Usage: table-processor --input-file <input.csv> [options]")
    } else if (arguments(0) != "--input-file") {
      Left("The first option must be --input-file")
    } else {
      val filterArguments = ArrayBuffer.empty[String]
      var separator = defaultSeparator
      var index = 2

      while (index < arguments.length) {
        if (arguments(index) == "--input-separator") {
          if (index + 1 >= arguments.length) {
            return Left("--input-separator requires a value")
          }

          separator = arguments(index + 1)
          if (separator.isEmpty) {
            return Left("Separator cannot be empty")
          }
          index += 2
        } else {
          filterArguments.addOne(arguments(index))
          index += 1
        }
      }

      FilterParser.parse(filterArguments.toVector).map { filters =>
        CommandLineOptions(arguments(1), separator, filters)
      }
    }
  }
}
