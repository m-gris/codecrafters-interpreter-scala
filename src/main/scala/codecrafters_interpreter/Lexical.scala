package codecrafters_interpreter

import scala.deriving.Mirror
import scala.compiletime.summonAll
import scala.compiletime.ops.string.Length
import scala.compiletime.ops.int.{<=, >=}


sealed trait LexicalElement:
  def sourceText: String

opaque type Spelling = String

object Spelling:
  def apply
  // Singleton: S is the literal's own type ("=="), not String.
  // Only a literal type has a length the compiler can compute.
  [S <: String & Singleton](s: S)
  // A proof the compiler must find: Length[S] <= 2 reduces to the type
  // true or false, and =:= only has an instance when both sides are the
  // same type. No proof, no compile: Spelling("...") is rejected.
  // Likewise Spelling("") is rejected: an empty spelling would match
  // everywhere while consuming nothing, and scanning would never end.
  (using (Length[S] >= 1) =:= true, (Length[S] <= 2) =:= true): Spelling = s

  extension (l: Spelling)
    // Not `chars`: where Spelling is transparent, String's own chars() (an IntStream) would win.
    def characters: List[Char] = l.toList
    def text: String = l

sealed trait FixedSpelling:
  def spelling: Spelling
  // A fixed spelling is the pattern, and every lexeme matching it is that same text, so the
  // source text is derived from the spelling, never stored. final: no element can say otherwise.
  final def sourceText: String = spelling.text

type Input = List[Char]
type Lexeme = List[Char]
type Remainder = List[Char]

// One successful step of a lexer, at the front of the input: the element it recognised, its lexeme
// (the characters it consumed), and what's left. The lexeme followed by the remainder is the input.
case class LexerMatch[+A](element: A, lexeme: Lexeme, remainder: Remainder)

// A Lexer[A] is a function Input => Option[LexerMatch[A]]: a match at the front of the input, or None.
// Kept as a trait with a named method rather than extending Function1, whose andThen/compose would read as lexer combinators.
trait Lexer[+A]:
  def lex(src: Input): Option[LexerMatch[A]]

object Lexer:
  // Called by `derives Lexer`: a sealed type's lexer is the longest match among its children's lexers.
  inline def derived[A](using m: Mirror.SumOf[A]): Lexer[A] =
    // The cast is safe: every child of a sealed A is a subtype of A, and Lexer is covariant.
    // The Mirror doesn't expose that relation to the type checker, so it can't prove it here.
    val lexers: List[Lexer[A]] = summonAll[Tuple.Map[m.MirroredElemTypes, Lexer]].toList.asInstanceOf[List[Lexer[A]]]
    longest(lexers)


sealed trait Recognized extends LexicalElement derives Lexer
sealed trait Token extends Recognized derives Lexer
sealed trait InSignificant extends Recognized derives Lexer

// A second classification, beside Token and Malformed: elements read by the string rule
// (a quote, then text up to the next quote or the end of the input). Not a child of
// Recognized: Literal and Malformed derive their lexers from their own children, so each
// string form also has a given that narrows Quoted.lexer to that form.
sealed trait Quoted

object Quoted:

  val quote: Char = '"'

  def addQuotes(value: String): String = s"$quote$value$quote"
  def addQuotes(value: List[Char]): List[Char] = quote +: value :+ quote

  given lexer: Lexer[Quoted] with

    def lex(src: Input): Option[LexerMatch[Quoted]] = src match
      case `quote` :: afterOpeningQuote =>
        val (content, fromClosingQuote) = afterOpeningQuote.span(_ != quote)
        fromClosingQuote match
          case `quote` :: afterClosingQuote => Some(LexerMatch(Literal.Str(content.mkString), addQuotes(content), afterClosingQuote))
          case _                            => Some(LexerMatch(UnterminatedStr(content.mkString), quote :: content, Nil))
      case _ => None



sealed trait Malformed extends Recognized derives Lexer

case class UnterminatedStr(content: String) extends Malformed with Quoted:
  def sourceText: String = Quoted.quote +: content


object UnterminatedStr:

  given Lexer[UnterminatedStr] with

      def lex(src: Input): Option[LexerMatch[UnterminatedStr]] = Quoted.lexer.lex(src).collect {
        case LexerMatch(u: UnterminatedStr, lexeme, remainder) => LexerMatch(u, lexeme, remainder)
      }


case class Comment(content: String) extends InSignificant:
  val sourceText: String = Comment.delimiter.mkString ++ this.content

case class UnRecognized(char: Char) extends LexicalElement:
  val sourceText: String = char.toString

// The elements that are errors: input no lexer recognised, and input a lexer recognised as broken.
// They have no common parent, so the union names them; a match on it is still checked for completeness.
type LexicalError = UnRecognized | Malformed


object Comment:

  val delimiter: List[Char] = List('/', '/')
  val terminator: WhitespaceChar = WhitespaceChar.NewLine

  given Lexer[Comment] with

    def lex(src: Input): Option[LexerMatch[Comment]] =
      if src.startsWith(delimiter) then
        val rest = src.drop(delimiter.size)
        val (content, restExComment) = rest.span(_ != terminator.char)
        Some(LexerMatch(Comment(content.mkString), delimiter ::: content, restExComment))
      else None



enum WhitespaceChar(val spelling: Spelling) extends InSignificant, FixedSpelling:

  case Space extends WhitespaceChar(Spelling(" "))
  case NewLine extends WhitespaceChar(Spelling("\n"))
  case Tab extends WhitespaceChar(Spelling("\t"))
  case CarriageReturn extends WhitespaceChar(Spelling("\r"))

  // head is safe: Spelling rejects an empty literal at compile time.
  def char: Char = spelling.characters.head

object WhitespaceChar:
  given Lexer[WhitespaceChar] = longestPrefixMatch(WhitespaceChar.values.toList)

def longest[A](lexers: List[Lexer[A]]): Lexer[A] =
  (src: Input) => lexers
                    .flatMap(recog => recog.lex(src))
                    .maxByOption(_.lexeme.size)


object Recognized:

  object LongestMatch:
    def unapply(src: Input): Option[LexerMatch[Recognized]] = summon[Lexer[Recognized]].lex(src)


enum Punctuator(val spelling: Spelling) extends Token, FixedSpelling:
  case LeftParen extends Punctuator(Spelling("("))
  case LeftBrace extends Punctuator(Spelling("{"))
  case RightParen extends Punctuator(Spelling(")"))
  case RightBrace extends Punctuator(Spelling("}"))
  case Comma extends Punctuator(Spelling(","))
  case Dot extends Punctuator(Spelling("."))
  case Minus extends Punctuator(Spelling("-"))
  case Plus extends Punctuator(Spelling("+"))
  case Semicolon extends Punctuator(Spelling(";"))
  case Star extends Punctuator(Spelling("*"))
  case Equal extends Punctuator(Spelling("="))
  case EqualEqual extends Punctuator(Spelling("=="))
  case Bang extends Punctuator(Spelling("!"))
  case BangEqual extends Punctuator(Spelling("!="))
  case Less extends Punctuator(Spelling("<"))
  case LessEqual extends Punctuator(Spelling("<="))
  case Greater extends Punctuator(Spelling(">"))
  case GreaterEqual extends Punctuator(Spelling(">="))
  case Slash extends Punctuator(Spelling("/"))


object Punctuator:

  given Lexer[Punctuator] = longestPrefixMatch(Punctuator.values.toList)

sealed trait Literal extends Token derives Lexer

object Literal:

  // Holds its source text, and derives the value from it: "1.50" and "1.5" have the same value,
  // so the text cannot be rebuilt from the value.
  case class Num(sourceText: String) extends Literal:
    // A val, so that a Num whose text is not a number fails when it is created.
    val value: Double = sourceText.toDouble

  object Num:
    given Lexer[Num] with
      def lex(src: Input): Option[LexerMatch[Num]] =
        src.span(_.isDigit) match
          case (Nil, _) => None
          case (integers, tail) if !tail.startsWith(".") => Some(LexerMatch(Num(integers.mkString), integers, tail))
          case (integers, tail) if  tail.startsWith(".") => tail match
            case '.' :: Nil => Some(LexerMatch(Num(integers.mkString), integers, tail))
            case '.' :: newTail => newTail.span(_.isDigit) match
              case (Nil, _) => Some(LexerMatch(Num(integers.mkString), integers, tail))
              case (fractions, remainder) =>
                Some(LexerMatch(Num(s"${integers.mkString}.${fractions.mkString}"), s"${integers.mkString}.${fractions.mkString}".toList, remainder))
            case _ => None
          case _ => None




  case class Str(val value: String) extends Literal with Quoted:
    def sourceText: String = Quoted.addQuotes(value)

  object Str:

    given Lexer[Literal.Str] with
      def lex(src: Input): Option[LexerMatch[Str]] = Quoted.lexer.lex(src).collect {
          case LexerMatch(s: Str, lexeme, remainder) => LexerMatch(s, lexeme, remainder)
      }




def longestPrefixMatch[A <: FixedSpelling](candidates: List[A]): Lexer[A] = (input: Input) =>
  candidates.filter(c => input.startsWith(c.spelling.characters)).maxByOption(_.spelling.characters.size)
    .map(winner => LexerMatch(winner, winner.spelling.characters, input.drop(winner.spelling.characters.size)))
