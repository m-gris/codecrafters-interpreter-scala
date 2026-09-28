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

  test("two single-char tokens side by side are both kept") {
    val source = Source("()")
    val expected = ScanResult(Nil, List(Token.LeftParen, Token.RightParen, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected, s"""\n  source   = "$source"\n  expected = ${expected.tokens}\n  obtained = ${actual.tokens}""")
  }

  test("a multi-token source yields one token per character, in source order") {
    val source = Source("())")
    val expected = ScanResult(Nil, List(Token.LeftParen, Token.RightParen, Token.RightParen, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected, s"""\n  source   = "$source"\n  expected = ${expected.tokens}\n  obtained = ${actual.tokens}""")
  }


  test("an unrecognised character is reported and does not stop scanning") {
    val expected = ScanResult(
      errors = List(ScanError("@")),
      tokens = List(Token.EndOfFile)
    )
    assertEquals(scan(Source("@")), expected)
  }

  test("an unrecognised character is reported and does not stop scanning") {
    val expected = ScanResult(
      errors = List(ScanError("@")),
      tokens = List(Token.LeftParen,Token.EndOfFile)
    )
    assertEquals(scan(Source("@(")), expected)
  }

  test("EqualEqual is recognized") {
    val source = Source("==")
    val expected = ScanResult( errors=Nil, tokens=List(Token.EqualEqual, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected)
  }
  test("Equal is recognized") {
    val source = Source("=")
    val expected = ScanResult( errors=Nil, tokens=List(Token.Equal, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected)
  }

  test("the longest lexeme wins: === is EqualEqual then Equal") {
    val source = Source("===")
    val expected = ScanResult( errors=Nil, tokens=List(Token.EqualEqual, Token.Equal, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected, s"""\n  source   = "$source"\n  expected = ${expected.tokens}\n  obtained = ${actual.tokens}""")
  }




