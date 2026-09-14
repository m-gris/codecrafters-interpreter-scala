package codecrafters_interpreter

enum TokenType:
  case LeftParen, RightParen, EndOfFile

opaque type Source = String
object Source:
  def apply(s: String): Source = s

opaque type Lexeme = String
object Lexeme:
  def apply(s: String): Lexeme = s


case class Token(tokenType: TokenType, lexeme: Lexeme)

def scan(source: Source): List[Token] = List(Token(TokenType.EndOfFile, Lexeme("")))

