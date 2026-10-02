package codecrafters_interpreter

import scala.compiletime.ops.string.Length
import scala.compiletime.ops.int.<=


sealed trait LexicalElement

sealed trait Recognized extends LexicalElement
sealed trait Significant extends Recognized
sealed trait InSignificant extends Recognized
case class Comment(text: String) extends InSignificant

case class UnRecognized(char: Char) extends LexicalElement

type Input = List[Char]
type Consumed = List[Char]
type Remainder = List[Char]

trait Lexer[+A]:
  def lex(src: Input): Option[(A, Consumed, Remainder)]


object Comment:

  given Lexer[Comment] with

    def lex(src: Input): Option[(Comment, Consumed, Remainder)] = src match

      case '/' :: '/' :: rest =>
        val (content, restExComment) = rest.span(_ != '\n')
        Some((Comment(content.mkString), '/' :: '/':: content, restExComment))

      case _ => None


enum WhitespaceChar(val char: Char) extends InSignificant:
  case Space extends WhitespaceChar(' ')
  case NewLine extends WhitespaceChar('\n')
  case Tab extends WhitespaceChar('\t')
  case CarriageReturn extends WhitespaceChar('\r')

object WhitespaceChar:

  private val reverseMap: Map[Char, WhitespaceChar] =
      WhitespaceChar.values.map(variant => variant.char -> variant).toMap

  def byChar(c: Char): Option[WhitespaceChar] = reverseMap.get(c)

  given Lexer[WhitespaceChar] with
    def lex(src: Input): Option[(WhitespaceChar, Consumed, Remainder)] = src match
      case c :: rest => WhitespaceChar.byChar(c).map( w => (w, List(c), rest))
      case _ => None



object Recognized:

  val recognizers: List[Lexer[Recognized]] = List(
    summon[Lexer[Comment]],
    summon[Lexer[Token]],
    summon[Lexer[WhitespaceChar]],
  )

  def longestMatch(recognizers: List[Lexer[Recognized]] )(src: Input): Option[(LexicalElement, Consumed, Remainder)] =
      val all: List[(LexicalElement, Consumed, Remainder)] = recognizers.flatMap(recog => recog.lex(src))
      all.maxByOption(_._2.size)

  object LongestMatch:
    def unapply(src: Input): Option[(LexicalElement, Consumed, Remainder)] = longestMatch(recognizers)(src)




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


enum Token(val lexeme: Lexeme) extends Significant:
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
  case Slash extends Token(Lexeme("/"))

val byLexeme: Map[Lexeme, Token] =
  Token.values.map(t => t.lexeme -> t).toMap

private def tokenFor(s: String): Option[Token] = byLexeme.get(s)

object Token:

  given Lexer[Token] with
    def lex(src: Input): Option[(Token, Consumed, Remainder)] = src match

      case Nil => None

      case x :: y :: rest => (tokenFor(x.toString), tokenFor(List(x, y).mkString)) match

          case (None, None) => None
          case (_ , Some(xyt)) => Some((xyt, List(x, y), rest))
          case (Some(xt), None) => Some((xt, List(x), y::rest))

      case x :: rest => tokenFor(x.toString).map(token => (token, List(x), rest))
