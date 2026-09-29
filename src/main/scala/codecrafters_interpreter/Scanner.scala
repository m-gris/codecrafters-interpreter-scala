package codecrafters_interpreter

import scala.compiletime.ops.string.Length
import scala.compiletime.ops.int.<=

opaque type Lexeme = String

object Lexeme:
  def apply
      // Singleton: S is the literal's own type ("=="), not String.
      // Only a literal type has a length the compiler can compute.
      [S <: String & Singleton]
      (s: S)
      // A proof the compiler must find: Length[S] <= 2 reduces to the type
      // true or false, and =:= only has an instance when both sides are the
      // same type. No proof, no compile: Lexeme("...") is rejected.
      (using (Length[S] <= 2) =:= true)
      : Lexeme = s


enum Token(val lexeme: Lexeme):
  case LeftParen extends Token(Lexeme("("))
  case LeftBrace extends Token(Lexeme("{"))
  case RightParen extends Token(Lexeme(")"))
  case RightBrace extends Token(Lexeme("}"))
  case Comma extends Token(Lexeme(","))
  case Dot extends Token(Lexeme("."))
  case Minus extends Token(Lexeme("-"))
  case Plus extends Token(Lexeme("+"))
  case SemiColon extends Token(Lexeme(";"))
  case Star extends Token(Lexeme("*"))
  case Equal extends Token(Lexeme("="))
  case EqualEqual extends Token(Lexeme("=="))

opaque type Source = String
object Source:
  def apply(s: String): Source = s

private val byLexeme: Map[String, Token] = Token.values.map(t => t.lexeme -> t).toMap

case class ScanError(unexpected: Char)
case class ScanResult(errors: List[ScanError], tokens: List[Token])

def scan(source: Source): ScanResult =

  def helper(src: List[Char]): ScanResult = src match

    case Nil              => ScanResult(errors = Nil, tokens = List() )

    case x :: Nil         => byLexeme.get(x.toString) match
      case None => ScanResult(errors=List(ScanError(x)), tokens=List())
      case Some(t) => ScanResult(errors=Nil, tokens=List(t))

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

