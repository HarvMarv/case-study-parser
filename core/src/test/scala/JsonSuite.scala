package parser

import Json._
import hedgehog._
import hedgehog.munit.HedgehogSuite

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

  val genNumber: Gen[(String, Json)] = Gen
    .list(Gen.digit, Range.linear(1, 9))
    .map(list => list.foldLeft("") { (str, char) => str :+ char })
    .map(num => (num, JsonNumber(num.toInt)))

  val genString: Gen[(String, Json)] = Gen
    .string(Gen.alphaNum, Range.linear(0, 35))
    .map(str => (jsonStringify(str), JsonString(str)))

  val genWhitespace: Gen[String] =
    Gen.int(Range.linear(0, 50)).map(count => " " * count)

  val genBool: Gen[(String, Json)] = Gen.element1("true", "false").map(bool => if (bool == "true") (bool, JsonBoolean(true)) else (bool, JsonBoolean(false)))
  
  val genNull: Gen[(String, Json)] =
    Gen.constant("null").map(_ => ("null", JsonNull()))
    
  val genArray: Gen[(String, Json)] = 
    for {
      leftBracket <- Gen.constant("[")
      result <- Gen.list(genValue, Range.linear(0, 10)).map{ list => 
        val (strings, jsons) = 
          list.foldLeft((List.empty[String], List.empty[Json])){ (accum, elt) =>
            val (string, json) = elt  
            val (strings, jsons) = accum

            (string :: strings, json :: jsons)
          }
      
        (strings.mkString(", "), jsons)
      }
      (string, json) = result
      rightBracket <- Gen.constant("]")
      input = leftBracket ++ string ++ rightBracket
    } yield (input, JsonArray(json))

  val genValue: Gen[(String, Json)] = 
    Gen.choice1(genString, genNumber, genBool, genNull, genArray)

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


  property("jsonNumber succeeds with expected value") {
    for {
      (string, json) <- genNumber.forAll
      suffix <- Gen.string(Gen.alpha, Range.linear(0, 35)).forAll
      input = string ++ suffix
    } yield {
      val result: parser.Result[Json.Json] = jsonNumber.parse(input)
      val expected: parser.Result[Json.Json] = parser.Success(
        json,
        input,
        string.length()
      )
      Result.diff(result, expected)(_ == _)
    }
  }

  property("jsonString succeeds with expected result") {
    for {
      (prefix, json) <- genString.forAll
      suffix <- Gen.string(Gen.latin1, Range.linear(0, 35)).forAll
      input = prefix ++ suffix
    } yield {
      val result: parser.Result[Json.Json] = jsonString.parse(input)
      val expected: parser.Result[Json.Json] =
        parser.Success(json, input, (prefix.length))
      Result.diff(result, expected)(_ == _)
    }
  }

  property("jsonValue succeeds with expected value") {
    for {
      space1 <- genWhitespace.forAll
      space2 <- genWhitespace.forAll
      (json, parsedJson) <- Gen.choice1(genString, genNumber, genBool, genNull, genArray).forAll
      _ = println(json)
      input = space1 ++ json ++ space2
    } yield {
      val result: parser.Result[Json.Json] = jsonValue.parse(input)
      print("\nValue tested: ")
      println(input)
      print("Result: ")
      println(result)

      result match {
        case parser.Success(parsed, _, _) => Result.diff(parsed, parsedJson)(_ == _)
        case parser.Failure(_, _, _) => fail(s"Parsing failed on input $input")
      }
    }
  }

  // property("jsonObject succeeds with empty object") {
  //   for {
  //     leftBracket <- Gen.constant("{").forAll
  //     whitespace <- genWhitespace.forAll
  //     rightBracket <- Gen.constant("}").forAll
  //     input = leftBracket ++ whitespace ++ rightBracket
  //   } yield {
  //     val result: parser.Result[Json.Json] = jsonObject.parse(input)
  //     val expected: parser.Result[Json.Json] =
  //       parser.Success(JsonObject(Map()), input, input.length())
  //     assertEquals(result, expected)
  //   }
  // }

  //   property("jsonArray succeeds with empty object") {
  //   for {
  //     leftBracket <- Gen.constant("[").forAll
  //     whitespace <- genWhitespace.forAll
  //     rightBracket <- Gen.constant("]").forAll
  //     input = leftBracket ++ whitespace ++ rightBracket
  //   } yield {
  //     val result: parser.Result[Json.Json] = jsonArray.parse(input)
  //     val expected: parser.Result[Json.Json] =
  //       parser.Success(JsonArray(List.empty), input, input.length())
  //     assertEquals(result, expected)
  //   }
  // }


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
