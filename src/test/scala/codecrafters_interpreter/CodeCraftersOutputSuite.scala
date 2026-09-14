//> using dep org.scalameta::munit::1.3.6

package codecrafters_interpreter

class CodeCraftersOutputSuite extends munit.FunSuite:

  test("render function") {
    assertEquals(Token.EndOfFile.render, "EOF  null")
    assertEquals(Token.LeftParen.render, "LEFT_PAREN ( null")
    assertEquals(Token.RightParen.render, "RIGHT_PAREN ) null")
  }
