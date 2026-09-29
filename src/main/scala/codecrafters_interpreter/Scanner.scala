package codecrafters_interpreter

import scala.compiletime.ops.string.Length
import scala.compiletime.ops.int.<=

opaque type Lexeme = String

object Lexeme:
  def apply
  // Singleton: S is the literal's own type ("=="), not String.
  // Only a literal type has a length the compiler can compute.
  [S <: String & Singleton](s: S)
  // A proof the compiler must find: Length[S] <= 2 reduces to the type
  // true or false, and =:= only has an instance when both sides are the
  // same type. No proof, no compile: Lexeme("...") is rejected.
  (using (Length[S] <= 2) =:= true): Lexeme = s

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
  case Bang extends Token(Lexeme("!"))
  case BangEqual extends Token(Lexeme("!="))
  case Less extends Token(Lexeme("<"))
  case LessEqual extends Token(Lexeme("<="))
  case Greater extends Token(Lexeme(">"))
  case GreaterEqual extends Token(Lexeme(">="))

opaque type Source = String
object Source:
  def apply(s: String): Source = s

private val byLexeme: Map[Lexeme, Token] =
  Token.values.map(t => t.lexeme -> t).toMap

case class ScanError(unexpected: Char)

case class ScanResult(errors: List[ScanError] = Nil, tokens: List[Token] = Nil):
  def add(e: ScanError): ScanResult = this.copy(errors= e :: this.errors)
  def add(t: Token): ScanResult = this.copy(tokens = t :: this.tokens)
  def reverse: ScanResult = ScanResult(this.errors.reverse, this.tokens.reverse)

def scan(source: Source): ScanResult =

  @scala.annotation.tailrec
  def loop(src: List[Char], acc: ScanResult): ScanResult = src match

    case Nil => acc

    case x :: Nil =>
      byLexeme.get(x.toString) match
        case None    => acc.add(ScanError(x))
        case Some(t) => acc.add(t)

    case x :: y :: tail =>

      (byLexeme.get(x.toString), byLexeme.get(List(x, y).mkString)) match

        case (None, None) => loop(y :: tail, acc.add(ScanError(x)) )

        case (_, Some(xyT)) => loop(tail, acc.add(xyT))

        case (Some(t), None) => loop(y :: tail, acc.add(t))

  val rez = loop(source.toList, ScanResult(Nil, Nil))

  rez.reverse
