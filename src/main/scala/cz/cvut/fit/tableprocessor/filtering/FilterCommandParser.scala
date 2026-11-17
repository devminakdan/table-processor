package cz.cvut.fit.tableprocessor.filtering

// A parsed filter and the number of values read after its flag.
final case class ParseResult(filter: TableFilter, consumedArgs: Int)

trait FilterCommandParser {
  // Returns the CLI flag handled by this filter type.
  def flag: String

  // Creates this filter from values following the command flag.
  def parse(args: Vector[String]): Either[String, ParseResult]
}
