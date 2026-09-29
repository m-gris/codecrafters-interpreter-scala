package codecrafters_interpreter

class ScannerSuite extends munit.FunSuite:

  // Printed by munit in place of "values are not the same", so a failure shows its input.
  private def clue(source: Source, expected: ScanResult, actual: ScanResult): String =
    s"""
       |  source   = "$source"
       |  expected = $expected
       |  obtained = $actual""".stripMargin

  test("empty source yields a single EndOfFile token") {
    val emptySource = Source("")
    val expected = ScanResult(Nil, List(Token.EndOfFile))
    val actual = scan(emptySource)
    assertEquals(actual, expected, clue(emptySource, expected, actual))
  }

  test("LeftParen does include EOF") {
    val source = Source("(")
    val expected = ScanResult(Nil, List(Token.LeftParen, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("RightParen does include EOF") {
    val source = Source(")")
    val expected = ScanResult(Nil, List(Token.RightParen, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("two single-char tokens side by side are both kept") {
    val source = Source("()")
    val expected = ScanResult(Nil, List(Token.LeftParen, Token.RightParen, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("a multi-token source yields its tokens in source order") {
    val source = Source("())")
    val expected = ScanResult(Nil, List(Token.LeftParen, Token.RightParen, Token.RightParen, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }


  test("an unrecognised character is reported and does not stop scanning") {
    val source = Source("@")
    val expected = ScanResult(
      errors = List(ScanError('@')),
      tokens = List(Token.EndOfFile)
    )
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("a token after an unrecognised character is still scanned") {
    val source = Source("@(")
    val expected = ScanResult(
      errors = List(ScanError('@')),
      tokens = List(Token.LeftParen,Token.EndOfFile)
    )
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("EqualEqual is recognized") {
    val source = Source("==")
    val expected = ScanResult( errors=Nil, tokens=List(Token.EqualEqual, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }
  test("Equal is recognized") {
    val source = Source("=")
    val expected = ScanResult( errors=Nil, tokens=List(Token.Equal, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("the longest lexeme wins: === is EqualEqual then Equal") {
    val source = Source("===")
    val expected = ScanResult( errors=Nil, tokens=List(Token.EqualEqual, Token.Equal, Token.EndOfFile))
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }




