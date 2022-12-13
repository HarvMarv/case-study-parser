/** Indicates the result of a parse. */
sealed trait Result[A]
/** The parse succeeded. 

    - result is the parsed value
    - input is the input that was parsed
    - offset is the index of where any remaining input starts.  */
final case class Success[A](result: A, input: String, offset: Int) extends Result[A]
/** The parse failed.

    - reason is a description of why the parser failed
    - input is the input that the parser attempted to parse
    - start is the index into input of where the parser started from */
final case class Failure[A](reason: String, input: String, start: Int) extends Result[A]