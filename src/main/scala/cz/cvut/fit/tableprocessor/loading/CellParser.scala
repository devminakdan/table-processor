package cz.cvut.fit.tableprocessor.loading

import cz.cvut.fit.tableprocessor.domain.Cell
import scala.util.Try

final class CellParser {
  def parse(rawValue: String): Either[String, Cell] = {
    val value = rawValue.trim

    if (value.isEmpty) {
      Right(Cell.Empty)
    } else if (value.startsWith("=")) {
      Right(Cell.Formula(value))
    } else {
      Try(BigInt(value)).toOption match {
        case Some(number) if number >= 0 => Right(Cell.Number(number))
        case _ => Left(s"Invalid cell value: '$rawValue'")
      }
    }
  }
}
