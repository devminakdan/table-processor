package cz.cvut.fit.tableprocessor.formula

import cz.cvut.fit.tableprocessor.domain.Coordinate

sealed trait Expression

object Expression {
  final case class NumberLiteral(value: BigInt) extends Expression

  final case class CellReference(coordinate: Coordinate) extends Expression

  final case class BinaryExpression(
      left: Expression,
      operator: BinaryOperator,
      right: Expression
  ) extends Expression
}
