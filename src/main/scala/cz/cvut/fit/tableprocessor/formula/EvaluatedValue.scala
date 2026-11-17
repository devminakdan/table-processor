package cz.cvut.fit.tableprocessor.formula

sealed trait EvaluatedValue

object EvaluatedValue {
  case object Empty extends EvaluatedValue

  final case class Number(value: BigInt) extends EvaluatedValue
}
