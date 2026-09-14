//> using dep org.scalameta::munit::1.3.6

package codecrafters_interpreter

class ScannerSuite extends munit.FunSuite:

  test("empty source yields a single EndOfFile token") {
    val emptySource = Source("")
    val expected = List(Token(TokenType.EndOfFile, Lexeme("")))
    val actual = scan(emptySource)
    assertEquals(actual, expected)
  }



