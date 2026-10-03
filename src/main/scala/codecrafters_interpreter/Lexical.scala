package codecrafters_interpreter

import scala.deriving.Mirror
import scala.compiletime.summonAll
import scala.compiletime.ops.string.Length
import scala.compiletime.ops.int.<=


sealed trait LexicalElement:
  def sourceText: String

sealed trait FixedSpelling:
  def spelling: List[Char]
  def sourceText: String = spelling.mkString

sealed trait Recognized extends LexicalElement derives Lexer
sealed trait Significant extends Recognized derives Lexer
sealed trait InSignificant extends Recognized derives Lexer
case class Comment(content: String) extends InSignificant:
  val sourceText: String = Comment.delimiter.mkString ++ this.content

case class UnRecognized(char: Char) extends LexicalElement:
  val sourceText: String = char.toString

type Input = List[Char]
type Consumed = List[Char]
type Remainder = List[Char]

// A Lexer[A] is a function Input => Option[(A, Consumed, Remainder)]: the A it recognised, what that consumed, what's left.
// Kept as a trait with a named method rather than extending Function1, whose andThen/compose would read as lexer combinators.
trait Lexer[+A]:
  def lex(src: Input): Option[(A, Consumed, Remainder)]

object Lexer:
  // Called by `derives Lexer`: a sealed type's lexer is the longest match among its children's lexers.
  inline def derived[A](using m: Mirror.SumOf[A]): Lexer[A] =
    // The cast is safe: every child of a sealed A is a subtype of A, and Lexer is covariant.
    // The Mirror doesn't expose that relation to the type checker, so it can't prove it here.
    val lexers: List[Lexer[A]] = summonAll[Tuple.Map[m.MirroredElemTypes, Lexer]].toList.asInstanceOf[List[Lexer[A]]]
    longest(lexers)


object Comment:

  val delimiter: List[Char] = List('/', '/')

  given Lexer[Comment] with

    def lex(src: Input): Option[(Comment, Consumed, Remainder)] =
      if src.startsWith(delimiter) then
        val rest = src.drop(delimiter.size)
        val (content, restExComment) = rest.span(_ != '\n')
        Some((Comment(content.mkString), delimiter ::: content, restExComment))
      else None



enum WhitespaceChar(val char: Char) extends InSignificant, FixedSpelling:

  case Space extends WhitespaceChar(' ')
  case NewLine extends WhitespaceChar('\n')
  case Tab extends WhitespaceChar('\t')
  case CarriageReturn extends WhitespaceChar('\r')

  def spelling = List(this.char)

object WhitespaceChar:
  given Lexer[WhitespaceChar] = longestPrefixMatch(WhitespaceChar.values.toList)

def longest[A](lexers: List[Lexer[A]]): Lexer[A] =
  (src: Input) => lexers
                    .flatMap(recog => recog.lex(src))
                    .maxByOption(_._2.size)


object Recognized:

  object LongestMatch:
    def unapply(src: Input): Option[(LexicalElement, Consumed, Remainder)] = summon[Lexer[Recognized]].lex(src)

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

  extension (l: Lexeme) def chars: List[Char] = l.toList


enum Token(val lexeme: Lexeme) extends Significant, FixedSpelling:
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

  def spelling = this.lexeme.chars


object Token:

  given Lexer[Token] = longestPrefixMatch(Token.values.toList)

def longestPrefixMatch[A <: FixedSpelling](candidates: List[A]): Lexer[A] = (input: Input) =>
  candidates.filter(c => input.startsWith(c.spelling)).maxByOption(_.spelling.size)
    .map(winner => (winner,winner.spelling,input.drop(winner.spelling.size)))



// val ALPHABET: List[Char]
