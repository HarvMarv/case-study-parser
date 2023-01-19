package parser

import cats._

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

  def and(that: => Parser[A])(implicit monoid: Monoid[A]): Parser[A] =
    ParserAnd(this, that, monoid)

  def repeat(implicit monoid: Monoid[A]): Parser[A] = ParserRepeat(this, monoid)

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

        case ParserAnd(source, that, monoid) =>
          loop(source, index) match {
            case Failure(reason, input, start) => Failure(reason, input, start)
            case Success(result1, _, offset) =>
              loop(that, offset) match {
                case Failure(reason, input, start) =>
                  Failure(reason, input, start)
                case Success(result2, input, offset) =>
                  Success(monoid.combine(result1, result2), input, offset)
              }
          }

        case ParserOrElse(source, that) =>
          loop(source, index) match {
            case Failure(_, _, _) => loop(that, index)
            case Success(result, input, offset) =>
              Success(result, input, offset)
          }

        case ParserRepeat(source, monoid) => {
          def repeatLoop(result: A, idx: Int): (A, Int) =
            loop(source, idx) match {
              case Failure(_, _, start) => (result, start)
              case Success(next, _, offset) =>
                repeatLoop(monoid.combine(result, next), offset)
            }
          val (result, offset) = repeatLoop(monoid.empty, index)
          Success(result, input, offset)
        }

        case ParserDelay(parser) => loop(parser(), index)
        case ParserFail()        => Failure("", input, index)
        case ParserPure(value)   => Success(value, input, index)
      }
    loop(this, 0)
  }
}

object Parser {
  implicit val parserInstance: Applicative[Parser] =
    new Applicative[Parser] {
      def pure[A](x: A): Parser[A] = Parser.pure(x)

      def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] =
        ff.product(fa).map { case (f, a) => f(a) }
    }

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
  final case class ParserRepeat[A](source: Parser[A], monoid: Monoid[A])
      extends Parser[A]
  final case class ParserAnd[A](
      source: Parser[A],
      that: Parser[A],
      monoid: Monoid[A]
  ) extends Parser[A]
  final case class ParserDelay[A](parser: () => Parser[A]) extends Parser[A]

  // Constructors
  // non-parser stuff => Parser

  def string(value: String): Parser[String] = ParserString(value)
  def pure[A](value: A): Parser[A] = ParserPure(value)
  def fail[A]: Parser[A] = ParserFail()
  def delay[A](parser: => Parser[A]): Parser[A] = ParserDelay(() => parser)

  val digit: Parser[String] =
    List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).foldLeft(Parser.fail[String]) {
      (parser, int) => parser.orElse(Parser.string(int.toString))
    }

  val twoDigits: Parser[String] = digit.and(digit)
  val fourDigits: Parser[String] = twoDigits.and(twoDigits)

  val integer: Parser[Int] = digit.and(digit.repeat).map(str => str.toInt)
}
