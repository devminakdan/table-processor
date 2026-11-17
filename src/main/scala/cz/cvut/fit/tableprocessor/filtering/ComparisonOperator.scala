package cz.cvut.fit.tableprocessor.filtering

trait ComparisonOperator {
  // Returns the operator text used in CLI filters.
  def symbol: String

  // Checks whether a cell value matches the target value.
  def matches(value: BigInt, target: BigInt): Boolean
}

object ComparisonOperator {
  case object LessThan extends ComparisonOperator {
    override val symbol: String = "<"
    override def matches(value: BigInt, target: BigInt): Boolean = value < target
  }

  case object GreaterThan extends ComparisonOperator {
    override val symbol: String = ">"
    override def matches(value: BigInt, target: BigInt): Boolean = value > target
  }

  case object LessThanOrEqual extends ComparisonOperator {
    override val symbol: String = "<="
    override def matches(value: BigInt, target: BigInt): Boolean = value <= target
  }

  case object GreaterThanOrEqual extends ComparisonOperator {
    override val symbol: String = ">="
    override def matches(value: BigInt, target: BigInt): Boolean = value >= target
  }

  case object Equal extends ComparisonOperator {
    override val symbol: String = "=="
    override def matches(value: BigInt, target: BigInt): Boolean = value == target
  }

  case object NotEqual extends ComparisonOperator {
    override val symbol: String = "!="
    override def matches(value: BigInt, target: BigInt): Boolean = value != target
  }

  val standard: Vector[ComparisonOperator] = Vector(
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual
  )
}
