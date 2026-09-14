package codecrafters_interpreter

enum Token(val lexeme: String):
  case LeftParen extends Token("(")
  case RightParen extends Token(")")
  case EndOfFile extends Token("")

opaque type Source = String
object Source:
  def apply(s: String): Source = s

def scan(source: Source): List[Token] = source.toList match
  case Nil  => List(Token.EndOfFile)
  case '(' :: rest => Token.LeftParen  :: scan(rest.mkString)
  case ')' :: rest => Token.RightParen :: scan(rest.mkString)
  case _ => ???

