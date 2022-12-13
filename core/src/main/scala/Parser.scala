sealed trait Parser[A] {
  import Parser._

  // Algebra:
  // - constructors: non-parser => Parser
  // - combinators: Parser + other stuff => Parser
  // - interpreter: Parser => non-parser

  // Reification implementation strategy for algebras:
  // - "reify" (turn into case classes) all constructors and combinators
  //   - all method parameters (including this) becomes constructor parameters to the case class
  //   - the parser extends the result type of the method
  // - implement the interpreter as a structural recursion

  // Combinators
  // Parser + other stuff => Parser

  def map[B](f: A => B): Parser[B] = ParserMap(this, f)

  def product[B](that: Parser[B]): Parser[(A, B)] = ParserProduct(this, that)

  def flatMap[B](f: A => Parser[B]): Parser[B] = ParserFlatMap(this, f)

  def orElse(that: => Parser[A]): Parser[A] = ParserOrElse(this, that)

  // Interpreter
  // Parser + other stuff => non-parser

  def parse(input: String): Result[A] = {
    def loop[A](parser: Parser[A], index: Int): Result[A] =
      parser match {
        case ParserString(value) =>
          if (input.startsWith(value, index))
            Success(value, input, index + value.size)
          else
            Failure(
              s"input did not start with $value at index $index",
              input,
              index
            )

        case ParserMap(source, f) =>
          loop(source, index) match {
            case Failure(reason, input, start) => Failure(reason, input, start)
            case Success(result, input, offset) =>
              Success(f(result), input, offset)

          }
        case ParserFlatMap(source, f) =>
          loop(source, index) match {
            case Failure(reason, input, start)  => Failure(reason, input, start)
            case Success(result, input, offset) => loop(f(result), offset)
          }
        case ParserProduct(source, that) =>
          loop(source, index) match {
            case Failure(reason, input, start) => Failure(reason, input, start)
            case Success(result1, _, offset) =>
              loop(that, offset) match {
                case Failure(reason, input, start) =>
                  Failure(reason, input, start)
                case Success(result2, input, offset) =>
                  Success((result1, result2), input, offset)
              }
          }
        case ParserOrElse(source, that) =>
          loop(source, index) match {
            case Failure(_, _, _) => loop(that, index)
            case Success(result, input, offset) =>
              Success(result, input, offset)
          }
        case ParserFail()      => Failure("", input, index)
        case ParserPure(value) => Success(value, input, index)
      }
    loop(this, 0)
  }
}

object Parser {
  final case class ParserString(value: String) extends Parser[String]
  final case class ParserMap[A, B](source: Parser[A], f: A => B)
      extends Parser[B]
  final case class ParserFlatMap[A, B](source: Parser[A], f: A => Parser[B])
      extends Parser[B]
  final case class ParserPure[A](value: A) extends Parser[A]
  final case class ParserProduct[A, B](source: Parser[A], that: Parser[B])
      extends Parser[(A, B)]
  final case class ParserOrElse[A](source: Parser[A], that: Parser[A])
      extends Parser[A]
  final case class ParserFail[A]() extends Parser[A]

  // Constructors
  // non-parser stuff => Parser

  def string(value: String): Parser[String] = ParserString(value)
  def pure[A](value: A): Parser[A] = ParserPure(value)
  def fail[A]: Parser[A] = ParserFail()

}
