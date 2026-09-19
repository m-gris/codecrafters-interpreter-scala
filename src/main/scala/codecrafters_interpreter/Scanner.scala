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

private val byLexeme: Map[Char, Token] = Token.values.collect {
  case t if t.lexeme.nonEmpty => t.lexeme.charAt(0) -> t
}.toMap

case class ScanError(unexpected: Char)
case class ScanResult(errors: List[ScanError], tokens: List[Token])

def scan(source: Source): ScanResult =

  def helper(src: List[Char]): ScanResult = src match
    case Nil         => ScanResult(errors = Nil, tokens = List(Token.EndOfFile) )
    case h :: rest   =>
      val headToken: Option[Token] = byLexeme.get(h)
      val tailTokens: ScanResult = helper(rest)
      headToken match {
        case Some(t) => ScanResult(tailTokens.errors, t :: tailTokens.tokens)
        case None => ScanResult( ScanError(h) :: tailTokens.errors, tailTokens.tokens)
      }

  helper(source.toList)

