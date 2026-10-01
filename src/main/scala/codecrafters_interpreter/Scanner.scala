package codecrafters_interpreter


opaque type Source = String
object Source:
  def apply(s: String): Source = s

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

    // WARNING the order of cases is more significant than it seems.
    case Nil => acc
    case Comment.Next(comment, rest)    => loop(rest, comment :: acc)
    case Token.Next(token, rest) => loop(rest, token :: acc)
    case WhitespaceChar.Next(whitespace, rest) => loop(rest, whitespace :: acc)
    case x :: rest => loop(rest, UnRecognized(x) :: acc)

  val rez = loop(source.toList, Nil)

  rez.reverse

def partition(elements: List[InputElement]): ScanResult =
  val rez = elements.foldLeft(ScanResult(Nil, Nil)) { (acc, e) => acc.add(e) } 
  rez.copy(rez.errors.reverse, rez.tokens.reverse)
