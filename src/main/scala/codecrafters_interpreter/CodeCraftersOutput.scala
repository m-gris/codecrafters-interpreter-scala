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

extension (t: Token) def render: String = s"${name(t)} ${t.sourceText} $NoLiteral"

extension (s: UnRecognized)
  def render: String = s"[line 1] Error: Unexpected character: ${s.char}"

extension (ts: List[Token])

  def render: List[String] =

    @scala.annotation.tailrec
    def loop(ts: List[Token], acc: List[String]): List[String] =

      ts match {
        case Nil       => "EOF  null" :: acc
        case t :: rest => loop(rest, s"${name(t)} ${t.sourceText} $NoLiteral" :: acc)
      }

    val rez = loop(ts, Nil)

    rez.reverse
