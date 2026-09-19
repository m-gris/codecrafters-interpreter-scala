//> using dep org.scalameta::munit::1.3.6

package codecrafters_interpreter

class ScannerSuite extends munit.FunSuite:

  test("empty source yields a single EndOfFile token") {
    val emptySource = Source("")
    val expected = ScanResult(Nil, List(Token.EndOfFile))
    val actual = scan(emptySource)
    assertEquals(actual, expected)
  }

  test("LeftParen does include EOF") {
    val source = Source("(")
    val expected = ScanResult(Nil, List(Token.LeftParen, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected)
  }

  test("RightParen does include EOF") {
    val source = Source(")")
    val expected = ScanResult(Nil, List(Token.RightParen, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected)
  }

  test("a multi-token source yields one token per character, in source order") {
    val source = Source("())")
    val expected = ScanResult(Nil, List(Token.LeftParen, Token.RightParen, Token.RightParen, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected)
  }


  test("an unrecognised character is reported and does not stop scanning") {
    val expected = ScanResult(
      errors = List(ScanError('@')),
      tokens = List(Token.EndOfFile)
    )
    assertEquals(scan(Source("@")), expected)
  }





