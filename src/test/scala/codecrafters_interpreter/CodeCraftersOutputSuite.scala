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
