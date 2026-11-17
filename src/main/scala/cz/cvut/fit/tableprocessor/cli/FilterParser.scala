package cz.cvut.fit.tableprocessor.cli

import cz.cvut.fit.tableprocessor.filtering.TableFilter
import cz.cvut.fit.tableprocessor.filtering.ComparisonFilter
import cz.cvut.fit.tableprocessor.filtering.EmptyCellFilter
import cz.cvut.fit.tableprocessor.filtering.NonEmptyCellFilter
import scala.collection.mutable.ArrayBuffer

final class FilterParser(registry: FilterCommandRegistry) {
  // Parses every filter command in their original CLI order.
  def parse(arguments: Vector[String]): Either[String, Vector[TableFilter]] = {
    val filters = ArrayBuffer.empty[TableFilter]
    var index = 0

    while (index < arguments.length) {
      registry.find(arguments(index)) match {
        case None => return Left(s"Unknown filter option: '${arguments(index)}'")
        case Some(commandParser) =>
          commandParser.parse(arguments.drop(index + 1)) match {
            case Left(error) => return Left(error)
            case Right(result) =>
              filters.addOne(result.filter)
              index += result.consumedArgs + 1
          }
      }
    }

    Right(filters.toVector)
  }
}

object FilterParser {
  private val standardRegistry = new FilterCommandRegistry(Vector(
    ComparisonFilter,
    EmptyCellFilter,
    NonEmptyCellFilter
  ))

  private val standardParser = new FilterParser(standardRegistry)

  // Parses the standard set of filter commands.
  def parse(arguments: Vector[String]): Either[String, Vector[TableFilter]] = {
    standardParser.parse(arguments)
  }
}
