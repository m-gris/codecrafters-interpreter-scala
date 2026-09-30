package codecrafters_interpreter

import scala.compiletime.ops.string.Length
import scala.compiletime.ops.int.<=


sealed trait InputElement

sealed trait Recognized extends InputElement
sealed trait Significant extends Recognized
sealed trait InSignificant extends Recognized

case class Comment(text: String) extends InSignificant

case class UnRecognized(char: Char) extends InputElement

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

opaque type Source = String
object Source:
  def apply(s: String): Source = s

private val byLexeme: Map[Lexeme, Token] =
  Token.values.map(t => t.lexeme -> t).toMap

// will deserve an educational comment about scala 2 scopes for types & terms etc...
type ScanError = UnRecognized
val ScanError = UnRecognized

case class ScanResult(errors: List[ScanError] = Nil, tokens: List[Token] = Nil):
  def add(e: InputElement): ScanResult =  e match {
    case t: Token     => this.copy(tokens= t :: this.tokens)
    case e: ScanError => this.copy(errors=e :: this.errors)
    case _ => this
  }

def scan(source: Source): List[InputElement] =

  @scala.annotation.tailrec
  def loop(src: List[Char], acc: List[InputElement]): List[InputElement] = src match

    case Nil => acc

    case x :: Nil =>
      byLexeme.get(x.toString) match
        case None    => ScanError(x) :: acc
        case Some(t) => t :: acc

    case '/' :: '/' :: rest =>
      val (commentContent, restExComment) = rest.span(_ != '\n')
      loop(restExComment, Comment(commentContent.mkString) :: acc)


    case x :: y :: tail =>

      (byLexeme.get(x.toString), byLexeme.get(List(x, y).mkString)) match

        case (None, None) => loop( y :: tail, ScanError(x) :: acc )

        case (_, Some(xyT)) => loop(tail, xyT :: acc)

        case (Some(t), None) => loop(y :: tail, t :: acc)

  val rez = loop(source.toList, Nil)

  rez.reverse

def partition(elements: List[InputElement]): ScanResult =
  val rez = elements.foldLeft(ScanResult(Nil, Nil)) { (acc, e) => acc.add(e) } 
  rez.copy(rez.errors.reverse, rez.tokens.reverse)
