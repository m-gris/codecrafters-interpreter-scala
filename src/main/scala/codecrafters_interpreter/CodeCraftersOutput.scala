package codecrafters_interpreter

// jlox prints its `literal` field, which is Java-null for any token with no literal value;
// Java renders that as "null".
// CodeCrafters froze jlox's output as the spec, so every implementation must emit it.
private val NoLiteral = "null"

private def toSnakeCase(s: String): String = s
  .flatMap { c =>
    if c.isUpper then s"_$c" else c.toUpper.toString
  }
  .stripPrefix("_")

private def name(t: Punctuator): String = toSnakeCase(t.toString)

// How a number's value is printed. The rule has no reason of its own: jlox prints a Java double
// with Double.toString ("42.0"), and the CodeCrafters tester imitates that with its own recipe
// (FormatFloat in its Go source): 6 decimals, trailing zeros removed, at least one decimal, and
// never scientific notation. Reproduced as it is.
private def printedValue(number: Double): String =
  // Locale.ROOT: the default locale could write a decimal comma ("42,000000") on some machines.
  val sixDecimals = String.format(java.util.Locale.ROOT, "%.6f", number)
  val withoutTrailingZeros = sixDecimals.substring(0, sixDecimals.lastIndexWhere(_ != '0') + 1)
  if withoutTrailingZeros.endsWith(".") then withoutTrailingZeros + "0" else withoutTrailingZeros


private def tokenLine(x: Token): String = x match
  case t: Punctuator => s"${name(t)} ${t.sourceText} $NoLiteral"
  case s: Literal.Str => s"""STRING ${s.sourceText} ${s.value}"""
  case n: Literal.Num => s"NUMBER ${n.sourceText} ${printedValue(n.value)}"

def stdoutLines(xs: List[Located[LexicalElement]]): List[String] =
  xs.collect { case Located(t: Token, _) => tokenLine(t) } ::: List("EOF  null")

private def errorLine(e: LexicalError, line: Int): String = e match {

  case u: UnRecognized =>
    s"[line ${line}] Error: Unexpected character: ${u.char}"

  case u: UnterminatedStr =>
    val extraNewLines: Int = u.sourceText.filter(_ == WhitespaceChar.NewLine.char).size
    s"[line ${line+extraNewLines}] Error: Unterminated string."

}

def stderrLines(xs: List[Located[LexicalElement]]): List[String] = xs.collect {
  case Located(e: LexicalError, line) => errorLine(e, line)
}



