package codecrafters_interpreter

// jlox prints its `literal` field, which is Java-null for any token with no literal value;
// Java renders that as "null".
// CodeCrafters froze jlox's output as the spec, so every implementation must emit it.
private val NoLiteral = "null"

extension (t: Token)
  def render: String = s"${t.name} ${t.lexeme} $NoLiteral"

