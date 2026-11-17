package cz.cvut.fit.tableprocessor.cli

import cz.cvut.fit.tableprocessor.filtering.FilterCommandParser

final class FilterCommandRegistry(parsers: Vector[FilterCommandParser]) {
  require(
    parsers.map(_.flag).distinct.length == parsers.length,
    "Each filter command flag must be unique"
  )

  private val parsersByFlag = parsers.map(parser => parser.flag -> parser).toMap

  // Finds the parser registered for one CLI flag.
  def find(flag: String): Option[FilterCommandParser] = parsersByFlag.get(flag)
}
