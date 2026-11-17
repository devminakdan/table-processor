package cz.cvut.fit.tableprocessor

import cz.cvut.fit.tableprocessor.cli.CommandLineOptions
import cz.cvut.fit.tableprocessor.domain.Table
import cz.cvut.fit.tableprocessor.filtering.TableFilter
import cz.cvut.fit.tableprocessor.formula.BinaryOperator
import cz.cvut.fit.tableprocessor.formula.FormulaEvaluator
import cz.cvut.fit.tableprocessor.formula.FormulaParser
import cz.cvut.fit.tableprocessor.formula.FormulaTokenizer
import cz.cvut.fit.tableprocessor.formula.TableEvaluator
import cz.cvut.fit.tableprocessor.loading.CellParser
import cz.cvut.fit.tableprocessor.loading.CsvTableParser
import cz.cvut.fit.tableprocessor.loading.FileTableSource
import cz.cvut.fit.tableprocessor.output.CsvTableFormatter
import java.nio.file.Path

object Main {
  private val tableSource = new FileTableSource(new CsvTableParser(new CellParser))
  private val tableEvaluator = new TableEvaluator(
    new FormulaTokenizer(BinaryOperator.standard),
    new FormulaParser,
    new FormulaEvaluator
  )
  private val formatter = new CsvTableFormatter

  def main(args: Array[String]): Unit = {
    process(args.toVector) match {
      case Right(output) => println(output)
      case Left(error) => Console.err.println(error)
    }
  }

  // Runs the current CSV, formula and filtering pipeline.
  private[tableprocessor] def process(arguments: Vector[String]): Either[String, String] = {
    CommandLineOptions.parse(arguments).flatMap { options =>
        tableSource
          .load(Path.of(options.inputFile), options.separator)
          .flatMap(tableEvaluator.evaluate)
          .map(table => formatter.format(applyFilters(table, options.filters), options.separator))
    }
  }

  // Applies each parsed filter in the same order as the command line.
  private def applyFilters(table: Table, filters: Vector[TableFilter]): Table = {
    var filteredTable = table
    var index = 0

    while (index < filters.length) {
      filteredTable = filters(index)(filteredTable)
      index += 1
    }

    filteredTable
  }
}
