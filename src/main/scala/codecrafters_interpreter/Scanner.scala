package codecrafters_interpreter


opaque type Source = String

object Source:
  def apply(s: String): Source = s

def scan(source: Source): List[LexicalElement] =

  @scala.annotation.tailrec
  def loop(src: List[Char], acc: List[LexicalElement]): List[LexicalElement] = src match

      case Nil => acc

      // Every kind of input element is tried; the longest match wins
      // so the order of the recognisers doesn't matter.
      case Recognized.LongestMatch(element, consumed, rest) => loop(rest, element :: acc)

      // Catch-all, kept as a plain list pattern on purpose: with Nil above, the compiler can prove
      // the match is exhaustive. An extractor here would hide that, since it may return None.
      case x :: rest => loop(rest, UnRecognized(x) :: acc)


  val rez = loop(source.toList, Nil)

  rez.reverse

def unscan(elements: List[LexicalElement]): Source = elements.map(_.sourceText).mkString


case class Located[+A](element: A, lineNumber: Int)

def locate(elements: List[LexicalElement]): List[Located[LexicalElement]] =

  elements.foldLeft( ( 1, List.empty[(LexicalElement, Int)] ) ) {
    case ( (count, acc)  , (elmt @ WhitespaceChar.NewLine)) => (count + 1, (elmt, count) :: acc )
    case ( (count, acc), elmt ) => (count, (elmt, count) :: acc)
  }._2.map((element, loc) => Located(element, loc)).reverse

