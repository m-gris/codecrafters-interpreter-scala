package codecrafters_interpreter

import scala.deriving.Mirror
import scala.compiletime.summonAll
import scala.compiletime.ops.string.Length
import scala.compiletime.ops.int.{<=, >=}


sealed trait LexicalElement:
  def sourceText: String

sealed trait FixedSpelling:
  def spelling: Spelling
  def sourceText: String = spelling.text

sealed trait Recognized extends LexicalElement derives Lexer
sealed trait Token extends Recognized derives Lexer
sealed trait InSignificant extends Recognized derives Lexer
case class Comment(content: String) extends InSignificant:
  val sourceText: String = Comment.delimiter.mkString ++ this.content

case class UnRecognized(char: Char) extends LexicalElement:
  val sourceText: String = char.toString

type Input = List[Char]
type Lexeme = List[Char]
type Remainder = List[Char]

// A Lexer[A] is a function Input => Option[(A, Lexeme, Remainder)]: the A it recognised, its lexeme (the characters it consumed), what's left.
// Kept as a trait with a named method rather than extending Function1, whose andThen/compose would read as lexer combinators.
trait Lexer[+A]:
  def lex(src: Input): Option[(A, Lexeme, Remainder)]

object Lexer:
  // Called by `derives Lexer`: a sealed type's lexer is the longest match among its children's lexers.
  inline def derived[A](using m: Mirror.SumOf[A]): Lexer[A] =
    // The cast is safe: every child of a sealed A is a subtype of A, and Lexer is covariant.
    // The Mirror doesn't expose that relation to the type checker, so it can't prove it here.
    val lexers: List[Lexer[A]] = summonAll[Tuple.Map[m.MirroredElemTypes, Lexer]].toList.asInstanceOf[List[Lexer[A]]]
    longest(lexers)


object Comment:

  val delimiter: List[Char] = List('/', '/')
  val terminator: WhitespaceChar = WhitespaceChar.NewLine

  given Lexer[Comment] with

    def lex(src: Input): Option[(Comment, Lexeme, Remainder)] =
      if src.startsWith(delimiter) then
        val rest = src.drop(delimiter.size)
        val (content, restExComment) = rest.span(_ != terminator.char)
        Some((Comment(content.mkString), delimiter ::: content, restExComment))
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
                    .maxByOption(_._2.size)


object Recognized:

  object LongestMatch:
    def unapply(src: Input): Option[(LexicalElement, Lexeme, Remainder)] = summon[Lexer[Recognized]].lex(src)

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


enum Punctuator(val spelling: Spelling) extends Token, FixedSpelling:
  case LeftParen extends Punctuator(Spelling("("))
  case LeftBrace extends Punctuator(Spelling("{"))
  case RightParen extends Punctuator(Spelling(")"))
  case RightBrace extends Punctuator(Spelling("}"))
  case Comma extends Punctuator(Spelling(","))
  case Dot extends Punctuator(Spelling("."))
  case Minus extends Punctuator(Spelling("-"))
  case Plus extends Punctuator(Spelling("+"))
  case SemiColon extends Punctuator(Spelling(";"))
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

def longestPrefixMatch[A <: FixedSpelling](candidates: List[A]): Lexer[A] = (input: Input) =>
  candidates.filter(c => input.startsWith(c.spelling.characters)).maxByOption(_.spelling.characters.size)
    .map(winner => (winner,winner.spelling.characters,input.drop(winner.spelling.characters.size)))



// val ALPHABET: List[Char]
