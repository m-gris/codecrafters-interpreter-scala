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


private def tokenLine(x: Token): String = x match
  case t: Punctuator => s"${name(t)} ${t.sourceText} $NoLiteral"
  case s: Literal.Str => s"""STRING ${s.sourceText} ${s.value}"""

def stdoutLines(xs: List[Located[LexicalElement]]): List[String] =
  xs.collect { case Located(t: Token, _) => tokenLine(t) } ::: List("EOF  null")

def stderrLines(xs: List[Located[LexicalElement]]): List[String] = xs.collect {
  case Located(u: UnRecognized, line) =>
    s"[line ${line}] Error: Unexpected character: ${u.char}"

  case Located(u: UnterminatedStr, line) =>
    val extraNewLines: Int = u.sourceText.filter(_ == WhitespaceChar.NewLine.char).size
    s"[line ${line+extraNewLines}] Error: Unterminated string."
}



