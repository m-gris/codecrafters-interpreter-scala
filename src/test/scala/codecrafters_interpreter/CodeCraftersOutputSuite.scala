package codecrafters_interpreter

class CodeCraftersOutputSuite extends munit.FunSuite:

  // Printed by munit in place of "values are not the same", so a failure shows its input elements.
  private def clue[A](elements: List[Located[LexicalElement]], expected: A, actual: A): String =
    s"""
       |  elements = $elements
       |  expected = $expected
       |  obtained = $actual""".stripMargin

  test("stdout: an empty source still prints EOF") {
    assertEquals(stdoutLines(Nil), List("EOF  null"))
  }

  test("stdout: one line per token, in order, then EOF") {
    assertEquals(
      stdoutLines(List(Located(Punctuator.LeftParen, 1), Located(Punctuator.RightParen, 1))),
      List("LEFT_PAREN ( null", "RIGHT_PAREN ) null", "EOF  null")
    )
  }

  test("stdout: every punctuator prints the token type name jlox uses") {
    // The names are derived from the Scala case names, so renaming a case would change the
    // output silently; this table pins them.
    val expected: Map[Punctuator, String] = Map(
      Punctuator.LeftParen -> "LEFT_PAREN",
      Punctuator.RightParen -> "RIGHT_PAREN",
      Punctuator.LeftBrace -> "LEFT_BRACE",
      Punctuator.RightBrace -> "RIGHT_BRACE",
      Punctuator.Comma -> "COMMA",
      Punctuator.Dot -> "DOT",
      Punctuator.Minus -> "MINUS",
      Punctuator.Plus -> "PLUS",
      Punctuator.Semicolon -> "SEMICOLON",
      Punctuator.Star -> "STAR",
      Punctuator.Equal -> "EQUAL",
      Punctuator.EqualEqual -> "EQUAL_EQUAL",
      Punctuator.Bang -> "BANG",
      Punctuator.BangEqual -> "BANG_EQUAL",
      Punctuator.Less -> "LESS",
      Punctuator.LessEqual -> "LESS_EQUAL",
      Punctuator.Greater -> "GREATER",
      Punctuator.GreaterEqual -> "GREATER_EQUAL",
      Punctuator.Slash -> "SLASH"
    )
    assertEquals(expected.keySet, Punctuator.values.toSet, "the table must cover every punctuator")
    expected.foreach { (p, name) =>
      assertEquals(stdoutLines(List(Located(p, 1))).head, s"$name ${p.sourceText} null")
    }
  }

  test("stdout: whitespace, comments and errors print nothing there") {
    val elements = List(
      Located(Punctuator.LeftParen, 1),
      Located(WhitespaceChar.NewLine, 1),
      Located(Comment(" hi"), 2),
      Located(UnRecognized('@'), 2)
    )
    assertEquals(stdoutLines(elements), List("LEFT_PAREN ( null", "EOF  null"))
  }

  test("stderr: one line per unrecognised character, with its line number") {
    val elements = List(
      Located(UnRecognized('@'), 1),
      Located(Punctuator.LeftParen, 1),
      Located(UnRecognized('#'), 3)
    )
    assertEquals(
      stderrLines(elements),
      List(
        "[line 1] Error: Unexpected character: @",
        "[line 3] Error: Unexpected character: #"
      )
    )
  }

  test("stdout: a string literal prints STRING, its source text with quotes, then its value") {
    val elements = List(Located(Literal.Str("foo baz"), 1))
    val expected = List("STRING \"foo baz\" foo baz", "EOF  null")
    val actual = stdoutLines(elements)
    assertEquals(actual, expected, clue(elements, expected, actual))
  }

  test("stdout and stderr: an unterminated string prints only an error, with its line number") {
    val elements = List(Located(UnterminatedStr("bar"), 1))
    val expectedOut = List("EOF  null")
    val actualOut = stdoutLines(elements)
    assertEquals(actualOut, expectedOut, clue(elements, expectedOut, actualOut))
    val expectedErr = List("[line 1] Error: Unterminated string.")
    val actualErr = stderrLines(elements)
    assertEquals(actualErr, expectedErr, clue(elements, expectedErr, actualErr))
  }

  test("stderr: an unterminated string is reported on the line where the input ends") {
    // jlox counts the newlines inside the string while reading it, then reports the error at the end.
    val elements = List(Located(UnterminatedStr("a\nb\nc"), 1))
    val expected = List("[line 3] Error: Unterminated string.")
    val actual = stderrLines(elements)
    assertEquals(actual, expected, clue(elements, expected, actual))
  }

  test("stdout: a number prints NUMBER, its source text, then its value with at least one decimal") {
    // The value's format is the tester's: 6 decimals at most, trailing zeros removed, at least
    // one decimal, never scientific notation.
    val cases = List(
      "42" -> "NUMBER 42 42.0",
      "1234.1234" -> "NUMBER 1234.1234 1234.1234",
      "65.0000" -> "NUMBER 65.0000 65.0",
      "100000000000000000000" -> "NUMBER 100000000000000000000 100000000000000000000.0"
    )
    for (text, line) <- cases do
      val elements = List(Located(Literal.Num(text), 1))
      val expected = List(line, "EOF  null")
      val actual = stdoutLines(elements)
      assertEquals(actual, expected, clue(elements, expected, actual))
  }

  test("stdout: many tokens do not overflow the stack") {
    // Plain recursion overflows the JVM stack at ~10,000 tokens.
    val lines = stdoutLines(List.fill(100_000)(Located(Punctuator.LeftParen, 1)))
    assertEquals(lines.size, 100_001)
    assertEquals(lines.last, "EOF  null")
  }
