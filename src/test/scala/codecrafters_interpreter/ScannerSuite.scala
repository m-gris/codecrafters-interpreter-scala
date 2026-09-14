//> using dep org.scalameta::munit::1.3.6

package codecrafters_interpreter

class ScannerSuite extends munit.FunSuite:

  test("empty source yields a single EndOfFile token") {
    val emptySource = Source("")
    val expected = List(Token.EndOfFile)
    val actual = scan(emptySource)
    assertEquals(actual, expected)
  }

  test("LeftParen does include EOF") {
    val source = Source("(")
    val expected = List(Token.LeftParen, Token.EndOfFile)
    val actual = scan(source)
    assertEquals(actual, expected)
  }

  test("RightParen does include EOF") {
    val source = Source(")")
    val expected = List(Token.RightParen, Token.EndOfFile)
    val actual = scan(source)
    assertEquals(actual, expected)
  }
