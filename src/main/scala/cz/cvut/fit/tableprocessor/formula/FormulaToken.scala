package cz.cvut.fit.tableprocessor.formula

import cz.cvut.fit.tableprocessor.domain.Coordinate

sealed trait FormulaToken

object FormulaToken {
  final case class Number(value: BigInt) extends FormulaToken

  final case class Reference(coordinate: Coordinate) extends FormulaToken

  final case class Operator(value: BinaryOperator) extends FormulaToken
}
