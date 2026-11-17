package cz.cvut.fit.tableprocessor.formula

trait BinaryOperator {
  def symbol: String

  // Defines the operator execution priority.
  def precedence: Int

  // Calculates the result for two numeric operands.
  def evaluate(left: BigInt, right: BigInt): Either[String, BigInt]
}

object BinaryOperator {
  case object Addition extends BinaryOperator {
    override val symbol: String = "+"

    override val precedence: Int = 1

    override def evaluate(left: BigInt, right: BigInt): Either[String, BigInt] =
      Right(left + right)
  }

  case object Subtraction extends BinaryOperator {
    override val symbol: String = "-"

    override val precedence: Int = 1

    override def evaluate(left: BigInt, right: BigInt): Either[String, BigInt] =
      Right(left - right)
  }

  case object Multiplication extends BinaryOperator {
    override val symbol: String = "*"

    override val precedence: Int = 2

    override def evaluate(left: BigInt, right: BigInt): Either[String, BigInt] =
      Right(left * right)
  }

  case object Division extends BinaryOperator {
    override val symbol: String = "/"

    override val precedence: Int = 2

    override def evaluate(left: BigInt, right: BigInt): Either[String, BigInt] = {
      if (right == 0) {
        Left("Division by zero")
      } else {
        Right(left / right)
      }
    }
  }

  val standard: Vector[BinaryOperator] = Vector(
    Addition,
    Subtraction,
    Multiplication,
    Division
  )
}
