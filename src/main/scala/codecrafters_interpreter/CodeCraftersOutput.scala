package codecrafters_interpreter

// jlox prints its `literal` field, which is Java-null for any token with no literal value;
// Java renders that as "null".
// CodeCrafters froze jlox's output as the spec, so every implementation must emit it.
private val NoLiteral = "null"

private def toSnakeCase(t: Token): String = t match {
  case Token.EndOfFile => "EOF"
  case Token.SemiColon => "SEMICOLON"
  case _  => t.toString.flatMap(
    c => if c.isUpper then s"_$c" else c.toUpper.toString
  ).stripPrefix("_")

  }


private def name(t: Token): String = t match {
  case t => toSnakeCase(t)
}

extension (t: Token)
  def render: String = s"${name(t)} ${t.lexeme} $NoLiteral"

