package cz.cvut.fit.tableprocessor.domain

// Formulas are stored as source text for now.
sealed trait Cell

object Cell {
  case object Empty extends Cell

  final case class Number(value: BigInt) extends Cell

  final case class Formula(source: String) extends Cell {
    require(source.startsWith("="), "A formula must start with '='")
  }
}
