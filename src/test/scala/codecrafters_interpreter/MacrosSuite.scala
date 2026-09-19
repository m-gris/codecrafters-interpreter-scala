package codecrafters_interpreter

// The property under test: a ScanError can only ever hold a character that no
// token uses. Checked at compile time, so it only applies to literal arguments.
class MacrosSuite extends munit.FunSuite:

  test("accepts a character no token uses") {
    assertEquals(ScanErrorMacro.checked('@').unexpected, '@')
  }

  test("rejects a character a token uses") {
    val errs = compileErrors("""ScanErrorMacro.checked('(')""")
    assert(errs.contains("legal token character"), errs)
  }

  // One more pair would strengthen the negative test. It currently only proves '(' is rejected.
  // Since the set is computed rather than hardcoded, a second character — say '*' rejected and '$' accepted — would show it really is the token set doing the discriminating, not a coincidence about
  // parens.

  test("rejects a character a token uses") {
    val errs = compileErrors("""ScanErrorMacro.checked('(')""")
    assert(errs.contains("legal token character"), errs)

    val errs2 = compileErrors("""ScanErrorMacro.checked('*')""")
    assert(errs2.contains("legal token character"), errs2)
  }


  test("accepts a character no token uses") {
    assertEquals(ScanErrorMacro.checked('$').unexpected, '$')
  }


