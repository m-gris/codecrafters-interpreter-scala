package codecrafters_interpreter

class CodeCraftersOutputSuite extends munit.FunSuite:

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
      Punctuator.SemiColon -> "SEMICOLON",
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

  test("stdout: many tokens do not overflow the stack") {
    // Plain recursion overflows the JVM stack at ~10,000 tokens.
    val lines = stdoutLines(List.fill(100_000)(Located(Punctuator.LeftParen, 1)))
    assertEquals(lines.size, 100_001)
    assertEquals(lines.last, "EOF  null")
  }
