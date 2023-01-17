import cats.implicits._

object Json extends App {

  sealed trait Json
  final case class JsonObject(pairs: Map[Json, Json]) extends Json
  final case class JsonArray(items: List[Json]) extends Json
  final case class JsonNumber(value: Int) extends Json
  final case class JsonString(value: String) extends Json
  final case class JsonBoolean(bool: Boolean) extends Json
  final case class JsonNull() extends Json

  val jsonNull: Parser[Json] = Parser.string("null").map(_ => JsonNull())

  val jsonBool: Parser[Json] =
    Parser
      .string("true")
      .orElse(Parser.string("false"))
      .map(boolStr => JsonBoolean(if (boolStr == "true") true else false))

  val digit: Parser[String] =
    List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).foldLeft(Parser.fail[String]) {
      (parser, int) => parser.orElse(Parser.string(int.toString))
    }

  val number: Parser[Json] =
    digit.and(digit.repeat).map(n => JsonNumber(n.toInt))

  val alphaLc =
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
  val alphaUc = alphaLc.map(_.toUpperCase())
  val punctuation = List(" ", ",", ":", "!", ".", "?", ";", "'")
  val stringMembers: Parser[String] =
    (alphaLc ++ alphaUc ++ punctuation).foldLeft(Parser.fail[String]) {
      (parser, char) =>
        parser.orElse(Parser.string(char))
    }

  val primitiveString: Parser[String] =
    stringMembers.and(stringMembers.repeat)
  val jsonString: Parser[Json] =
    (Parser.string("\""), primitiveString, Parser.string("\"")).mapN(
      (_, str, _) => JsonString(str)
    )

  val whitespace: Parser[String] = Parser.string(" ").repeat

  val value: Parser[Json] =
    (whitespace *> jsonString
      .orElse(number)
      .orElse(jsonBool)
      .orElse(jsonNull)
      .orElse(jsonArray) <* whitespace)

  val arrayItems: Parser[List[Json]] =
    (value, Parser.string(","), Parser.delay(arrayItems))
      .mapN((f, _, e) => f :: e)
      .orElse(value.map(List(_)))

  val jsonArray: Parser[Json] =
    (Parser.string("["), arrayItems, Parser.string("]")).mapN(
      (_, arrayItems, _) => JsonArray(arrayItems)
    )

  val keyValuePair: Parser[(Json, Json)] =
    (jsonString, Parser.string(":"), Parser.delay(value))
      .mapN((k, _, v) => (k, v))

  val objectMap: Parser[Map[Json, Json]] =
    (keyValuePair, Parser.string(","), Parser.delay(objectMap))
      .mapN((f, _, e) => Map(f._1 -> f._2) ++ e)
      .orElse(keyValuePair.map((x => Map(x._1 -> x._2))))
      // I added the possibility for whitespace to be the entire object (empty object)
      .orElse(whitespace.map(Map()))

  val jsonObject: Parser[Json] =
    (Parser.string("{"), objectMap, Parser.string("}")).mapN(
      (_, objectMap, _) => JsonObject(objectMap)
    )
}
