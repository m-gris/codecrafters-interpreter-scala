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
  case Equal extends Token("=")
  case EqualEqual extends Token("==")

opaque type Source = String
object Source:
  def apply(s: String): Source = s

private val byLexeme: Map[String, Token] = Token.values.collect {
  case t if t.lexeme.nonEmpty => t.lexeme -> t }.toMap

case class ScanError(unexpected: Char)
case class ScanResult(errors: List[ScanError], tokens: List[Token])

def scan(source: Source): ScanResult =

  def helper(src: List[Char]): ScanResult = src match

    case Nil              => ScanResult(errors = Nil, tokens = List(Token.EndOfFile) )

    case x :: Nil         => byLexeme.get(x.toString) match
      case None => ScanResult(errors=List(ScanError(x)), tokens=List(Token.EndOfFile))
      case Some(t) => ScanResult(errors=Nil, tokens=List(t, Token.EndOfFile))

    case x :: y :: tail => (byLexeme.get(x.toString), byLexeme.get(List(x,y).mkString)) match

      case (None, None)  =>
        val rest: ScanResult = helper(y :: tail)
        rest.copy(errors= ScanError(x) :: rest.errors)

      case (_ , Some(xyT)) =>
        val rest: ScanResult = helper(tail)
        rest.copy(tokens= xyT :: rest.tokens)

      case (Some(t), None) =>
        val rest: ScanResult = helper(y :: tail)
        rest.copy(tokens= t :: rest.tokens)

  helper(source.toList)

