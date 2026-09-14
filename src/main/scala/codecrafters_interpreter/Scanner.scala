package codecrafters_interpreter

enum Token(val lexeme: String, val name: String):
  case LeftParen extends Token("(", "LEFT_PAREN")
  case RightParen extends Token(")", "RIGHT_PAREN")
  case EndOfFile extends Token("", "EOF")

opaque type Source = String
object Source:
  def apply(s: String): Source = s

def scan(source: Source): List[Token] =

  def helper(src: List[Char]): List[Token] = src match
    case Nil  => List(Token.EndOfFile)
    case '(' :: rest => Token.LeftParen  :: helper(rest)
    case ')' :: rest => Token.RightParen :: helper(rest)
    case _ => ???


  helper(source.toList)

