package chapter9

sealed trait Result[+A] {
  def toEither: Either[ParseError, A] = this match {
    case Success(value, _) => Right(value)
    case Failure(error) => Left(error)
  }

  def toOption: Option[A] = toEither.toOption
}

case class Success[+A](value: A, charsConsumed: Int) extends Result[A]

case class Failure(error: ParseError) extends Result[Nothing]
