package codecrafters_interpreter

class CodeCraftersOutputSuite extends munit.FunSuite:

  test("render token") {
    assertEquals(List.empty[Token].render, List("EOF  null"))
    assertEquals(Token.LeftParen.render, "LEFT_PAREN ( null")
    assertEquals(Token.RightParen.render, "RIGHT_PAREN ) null")
  }

  test("render tokens: one line per token, in order, then EOF") {
    assertEquals(
      List(Token.LeftParen, Token.RightParen).render,
      List("LEFT_PAREN ( null", "RIGHT_PAREN ) null", "EOF  null")
    )
  }

  test("render scan error") {
    assertEquals(
      ScanError('@').render,
      """[line 1] Error: Unexpected character: @"""
    )
  }

  test("rendering many tokens does not overflow the stack") {
    // Plain recursion overflows the JVM stack at ~10,000 tokens.
    val lines = List.fill(100_000)(Token.LeftParen).render
    assertEquals(lines.size, 100_001)
    assertEquals(lines.last, "EOF  null")
  }
