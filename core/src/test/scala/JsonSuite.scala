package parser

import Json._
import hedgehog._
import hedgehog.munit.HedgehogSuite
import scala.util.Try

class JsonSuite extends HedgehogSuite {

  // property("string succeeds when input starts with expected value") {
  //   for {
  //     // We include 0 in the range because parsing an empty String is valid
  //     expected <- Gen.string(Gen.latin1, Range.linear(0, 10)).forAll
  //     suffix <- Gen.string(Gen.latin1, Range.linear(0, 35)).forAll
  //     input = expected ++ suffix
  //     result = Parser.string(expected).parse(input)
  //   } yield result match {
  //     case parser.Failure(_, _, _) =>
  //       println(s"Parser failed on input $input when it should have succeeded")
  //       fail(s"Parser failed on input $input")
  //     case parser.Success(output, _, _) =>
  //       assertEquals(output, expected)
  //   }
  // }

  def jsonStringify(str: String): String = "\"" ++ str ++ "\""

  def genNumber = Gen
    .list(Gen.digit, Range.linear(1, 9))
    .map(list => list.foldLeft("") { (str, char) => str :+ char })

  def genString = Gen.string(Gen.alphaNum, Range.linear(0, 35))

  val genWhitespace: Gen[String] =
    Gen.int(Range.linear(0, 50)).map(count => " " * count)

  test("jsonNull succeeds with expected value") {
    val input = "null"
    val result: parser.Result[Json.Json] = jsonNull.parse(input)
    val expected: parser.Result[Json.Json] =
      parser.Success(JsonNull(), input, 4)
    assertEquals(result, expected)
  }

  property("jsonNull succeeds with expected value") {
    for {
      input <- Gen.constant("null").forAll
    } yield {
      val result: parser.Result[Json.Json] = jsonNull.parse(input)
      val expected: parser.Result[Json.Json] =
        parser.Success(JsonNull(), input, 4)
      assertEquals(result, expected)
    }
  }

  property("jsonBool succeeds with expected value") {
    for {
      bool <- Gen.element1("true", "false").forAll
      suffix <- genWhitespace.forAll
      input = bool ++ suffix
    } yield {
      val result: parser.Result[Json.Json] = jsonBool.parse(input)
      val expected: parser.Result[Json.Json] =
        bool match {
          case "true"  => parser.Success(JsonBoolean(true), input, 4)
          case "false" => parser.Success(JsonBoolean(false), input, 5)
        }
      Result.diff(result, expected)(_ == _)
    }
  }

  property("whitespace succeeds with expected value") {
    for {
      whitespace <- genWhitespace.forAll
      suffix <- Gen.string(Gen.alphaNum, Range.linear(0, 10)).forAll
      input = whitespace ++ suffix
    } yield {
      val result: parser.Result[String] = parser.Json.whitespace.parse(input)
      val expected: parser.Result[String] =
        parser.Success(whitespace, input, whitespace.length)
      Result.diff(result, expected)(_ == _)
    }
  }

  property("jsonBool succeeds with expected value") {
    for {
      bool <- Gen.element1("true", "false").forAll
      suffix <- genWhitespace.forAll
      input = bool ++ suffix
    } yield {
      val result: parser.Result[Json.Json] = jsonBool.parse(input)
      val expected: parser.Result[Json.Json] =
        bool match {
          case "true"  => parser.Success(JsonBoolean(true), input, 4)
          case "false" => parser.Success(JsonBoolean(false), input, 5)
        }
      Result.diff(result, expected)(_ == _)
    }
  }

  property("jsonNumber succeeds with expected value") {
    for {
      prefix <- genNumber.forAll
      suffix <- Gen.string(Gen.alpha, Range.linear(0, 35)).forAll
      input = prefix ++ suffix
    } yield {
      val result: parser.Result[Json.Json] = jsonNumber.parse(input)
      val expected: parser.Result[Json.Json] = parser.Success(
        JsonNumber(prefix.toInt),
        input,
        prefix.length()
      )
      Result.diff(result, expected)(_ == _)
    }
  }

  property("jsonString succeeds with expected result") {
    for {
      prefix <- genString.forAll
      suffix <- Gen.string(Gen.latin1, Range.linear(0, 35)).forAll
      input = jsonStringify(prefix) ++ suffix
    } yield {
      val result: parser.Result[Json.Json] = jsonString.parse(input)
      val expected: parser.Result[Json.Json] =
        parser.Success(JsonString(prefix), input, (prefix.length() + 2))
      Result.diff(result, expected)(_ == _)
    }
  }

  property("jsonValue succeeds with expected value") {
    for {
      space1 <- genWhitespace.forAll
      space2 <- genWhitespace.forAll
      _baseString <- genString.forAll
      string = jsonStringify(_baseString)
      number <- genNumber.forAll
      bool <- Gen.element1("true", "false").forAll
      aNull <- Gen.constant("null").forAll
      // TODO: v Steal below generators for complex types
      // .orElse(jsonArray)
      // .orElse(jsonObject)

      _input <- Gen.element1(string, number, bool, aNull).forAll
      _ = println(_input)
      input = space1 ++ _input ++ space2
    } yield {
      val result: parser.Result[Json.Json] = jsonValue.parse(input)
      print("\nValue tested: ")
      println(input)
      print("Result: ")
      println(result)
      val expected: parser.Result[Json.Json] =
        string match {
          case "true"  => parser.Success(JsonBoolean(true), input, 4)
          case "false" => parser.Success(JsonBoolean(false), input, 5)
          case value if (Try { value.toInt }.isSuccess) =>
            parser.Success(JsonNumber(value.toInt), input, input.length)
            parser.Success(JsonString(value), input, value.length())
          case "null" =>
            parser.Success(JsonNull(), input, 4)
          case value if (value.startsWith("\"") && value.endsWith("\"")) =>
            parser.Success(
              JsonString(value.substring(1, value.length - 1)),
              input,
              input.length()
            )
        }
      Result.diff(result, expected)(_ == _)
    }
  }

  property("jsonObject succeeds with empty object") {
    for {
      leftBracket <- Gen.constant("{").forAll
      whitespace <- genWhitespace.forAll
      rightBracket <- Gen.constant("}").forAll
      input = leftBracket ++ whitespace ++ rightBracket
    } yield {
      val result: parser.Result[Json.Json] = jsonObject.parse(input)
      val expected: parser.Result[Json.Json] =
        parser.Success(JsonObject(Map()), input, input.length())
      assertEquals(result, expected)
    }
  }
  // match {
  //   case parser.Failure(_, _, _) =>
  //     println(s"Parser failed on input $input when it should have succeeded")
  //     fail(s"Parser failed on input $input")
  //   case parser.Success(result, _, _) => Result.diff(result, expected)(_ == _)
  // }

  // test("jsonObject succeeds with expected value".only) {
  //   val input = "{\"a\": \"b\"}"
  //   val result: parser.Result[Json.Json] = jsonObject.parse(input)
  //   val expected: parser.Result[Json.Json] =
  //     parser.Success(JsonObject(Map()), input, 4)
  //   Result.diff(result, expected)(_ == _)
  // }

  // property("null object succeeds") {
  //   val input = "null"
  //   val result = jsonNull.parse(input)
  //   val expected = parser.Success(JsonNull(), input, 4)

  //   withMunitAssertions { assertions =>
  //     assertions.assertEquals[parser.Result[Json], parser.Success[JsonNull]](
  //       result,
  //       expected
  //     )
  //   }

  // // PropertyT[parser.Result[String]]
  // PropertyT(result) match {
  //   case parser.Failure(_, _, _) =>
  //     fail(s"Parser failed on input $input when it should have succeeded")
  //   case parser.Success(_, _, _) => success
  // }
}
