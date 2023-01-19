package parser

import cats.implicits._

object Expression {
  // 1 + x
  // 2 * 3
  // y + z

  sealed trait Expression
  final case class Literal(value: Int) extends Expression
  final case class Variable(value: String) extends Expression
  final case class Add(left: Expression, right: Expression) extends Expression
  final case class Multiply(left: Expression, right: Expression)
      extends Expression

  val digit: Parser[String] =
    List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).foldLeft(Parser.fail[String]) {
      (parser, int) => parser.orElse(Parser.string(int.toString))
    }

  val alphabetic: Parser[String] =
    List(
      "a",
      "b",
      "c",
      "d",
      "e",
      "f",
      "g",
      "h",
      "i",
      "j",
      "k",
      "l",
      "m",
      "n",
      "o",
      "p",
      "q",
      "r",
      "s",
      "t",
      "u",
      "v",
      "w",
      "x",
      "y",
      "z"
    )
      .foldLeft(Parser.fail[String]) { (parser, char) =>
        parser.orElse(Parser.string(char))
      }

  val literal: Parser[Expression] =
    digit.and(digit.repeat).map(str => Literal(str.toInt))

  val variable: Parser[Expression] =
    alphabetic.and(alphabetic.repeat).map(Variable(_))

  // print(variable.parse("wholebigword"))
  val factor: Parser[Expression] = literal.orElse(variable)

  val whitespace: Parser[String] = Parser.string(" ").repeat

  // print(whitespace.parse("   wholebigword"))

  val plus: Parser[Unit] =
    // F[A] *> F[B]: F[B]
    // whitespace.*>(Parser.string("+"))
    // whitespace.productR(Parser.string("+"))
    (whitespace *> Parser.string("+") <* whitespace).void
  // whitespace.product(Parser.string("+")).product(whitespace).map(_ => ())

  val add: Parser[Expression] =
    (factor, plus, factor).mapN((l, _, r) => Add(l, r))
  // factor.product(plus).product(factor).map { case ((first, _), second) =>
  //   Add(first, second)
  // }

  val times: Parser[Unit] =
    whitespace.product(Parser.string("*")).product(whitespace).map(_ => ())

  val multiply: Parser[Expression] =
    factor.product(times).product(factor).map { case ((first, _), second) =>
      Multiply(first, second)
    }

  val term: Parser[Expression] =
    (factor, times, Parser.delay(term))
      .mapN((f, _, e) => Multiply(f, e): Expression)
      .orElse(factor)

  val expression: Parser[Expression] =
    (term, plus, Parser.delay(expression))
      .mapN((t, _, e) => Add(t, e): Expression)
      .orElse(term)

  println(expression.parse("52 * 2 + 2 + 2 + abc * g + 3"))
}
