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

private def name(t: Token): String = t match {
  case Token.SemiColon => "SEMICOLON"
  case t               => toSnakeCase(t.toString)
}


def stdoutLines(xs: List[Located[LexicalElement]]): List[String] = xs.collect {
  case Located(t: Token, line) => s"${name(t)} ${t.sourceText} $NoLiteral"
} ::: List("EOF  null")

def stderrLines(xs: List[Located[LexicalElement]]): List[String] = xs.collect {
  case Located(u: UnRecognized, line) =>
    s"[line ${line}] Error: Unexpected character: ${u.char}"
}



