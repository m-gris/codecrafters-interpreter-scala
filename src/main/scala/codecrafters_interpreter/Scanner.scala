package codecrafters_interpreter


opaque type Source = String

object Source:
  def apply(s: String): Source = s

case class ScanResult(errors: List[UnRecognized] = Nil, tokens: List[Token] = Nil):
  def add(e: LexicalElement): ScanResult =  e match {
    case t: Token     => this.copy(tokens= t :: this.tokens)
    case e: UnRecognized => this.copy(errors=e :: this.errors)
    case _ => this
  }


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

def partition(elements: List[LexicalElement]): ScanResult =
  val rez = elements.foldLeft(ScanResult(Nil, Nil)) { (acc, e) => acc.add(e) } 
  rez.copy(rez.errors.reverse, rez.tokens.reverse)
