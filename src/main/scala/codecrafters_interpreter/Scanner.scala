package codecrafters_interpreter

enum Token(val lexeme: String):
  case LeftParen extends Token("(")
  case LeftBrace extends Token("{")
  case RightParen extends Token(")")
  case RightBrace extends Token("}")
  case EndOfFile extends Token("")
  case Comma extends Token(",")
  case Dot extends Token(".")
  case Minus extends Token("-")
  case Plus extends Token("+")
  case SemiColon extends Token(";")
  case Star extends Token("*")

opaque type Source = String
object Source:
  def apply(s: String): Source = s

private val byLexeme: Map[String, Token] = Token.values.collect {
  case t if t.lexeme.nonEmpty => t.lexeme -> t
}.toMap

def scan(source: Source): List[Token] =

  def helper(src: List[Char]): List[Token] = src match
    case Nil         => List(Token.EndOfFile)
    case h :: rest   => byLexeme(h.toString) :: helper(rest)

  helper(source.toList)

