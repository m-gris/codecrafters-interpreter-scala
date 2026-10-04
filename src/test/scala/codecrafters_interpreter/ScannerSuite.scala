package codecrafters_interpreter


private val leafLexers: List[Lexer[Recognized]] =
  List(summon[Lexer[Comment]], summon[Lexer[Token]], summon[Lexer[WhitespaceChar]])

// Every value with a fixed spelling. A new FixedSpelling enum must be added here.
private val fixedSpellings: List[FixedSpelling] =
  Token.values.toList ++ WhitespaceChar.values.toList

// The characters generated inputs are drawn from: every character some lexer knows (taken from
// the spellings, so new tokens are included automatically; this also covers `//` via Slash),
// plus characters no lexer knows, so unrecognised input and its boundaries get exercised too.
private val alphabet: List[Char] =
  fixedSpellings.flatMap(_.spelling).distinct ++ List('@', 'a')

class ScannerSuite extends munit.FunSuite:

  // Printed by munit in place of "values are not the same", so a failure shows its input.
  private def clue(
      source: Source,
      expected: ScanResult,
      actual: ScanResult
  ): String =
    s"""
       |  source   = "$source"
       |  expected = $expected
       |  obtained = $actual""".stripMargin

  test("empty source yields an empty list") {
    val emptySource = Source("")
    val expected = ScanResult(Nil, List())
    val actual = partition(scan(emptySource))
    assertEquals(actual, expected, clue(emptySource, expected, actual))
  }

  test("two single-char tokens side by side are both kept") {
    val source = Source("()")
    val expected = ScanResult(Nil, List(Token.LeftParen, Token.RightParen))
    val actual = partition(scan(source))
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("a multi-token source yields its tokens in source order") {
    val source = Source("())")
    val expected =
      ScanResult(Nil, List(Token.LeftParen, Token.RightParen, Token.RightParen))
    val actual = partition(scan(source))
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("an unrecognised character is reported and does not stop scanning") {
    val source = Source("@")
    val expected = ScanResult(
      errors = List(UnRecognized('@')),
      tokens = List()
    )
    val actual = partition(scan(source))
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("a token after an unrecognised character is still scanned") {
    val source = Source("@(")
    val expected = ScanResult(
      errors = List(UnRecognized('@')),
      tokens = List(Token.LeftParen)
    )
    val actual = partition(scan(source))
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("EqualEqual is recognized") {
    val source = Source("==")
    val expected = ScanResult(errors = Nil, tokens = List(Token.EqualEqual))
    val actual = partition(scan(source))
    assertEquals(actual, expected, clue(source, expected, actual))
  }
  test("Equal is recognized") {
    val source = Source("=")
    val expected = ScanResult(errors = Nil, tokens = List(Token.Equal))
    val actual = partition(scan(source))
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("the longest lexeme wins: === is EqualEqual then Equal") {
    val source = Source("===")
    val expected =
      ScanResult(errors = Nil, tokens = List(Token.EqualEqual, Token.Equal))
    val actual = partition(scan(source))
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("the longest lexeme wins: !!=== is Bang, BangEqual, EqualEqual") {
    val source = Source("!!===")
    val expected = ScanResult(errors = Nil, tokens = List(Token.Bang, Token.BangEqual, Token.EqualEqual))
    val actual = partition(scan(source))
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("the longest lexeme wins: <<=>>= is Less, LessEqual, Greater, GreaterEqual") {
    val source = Source("<<=>>=")
    val expected = ScanResult(errors = Nil, tokens = List(Token.Less, Token.LessEqual, Token.Greater, Token.GreaterEqual))
    val actual = partition(scan(source))
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("a comment runs to the end of the line, whatever it contains") {
    val source = Source("(///Unicode:£§᯽☺♣)")
    val expected: List[LexicalElement] = List(Token.LeftParen, Comment("/Unicode:£§᯽☺♣)"))
    assertEquals(scan(source), expected)
  }

  test("scanning resumes at the newline that ends a comment") {
    val source = Source("// hi\n(")
    val expected: List[LexicalElement] = List(Comment(" hi"), WhitespaceChar.NewLine, Token.LeftParen)
    assertEquals(scan(source), expected)
  }

  test("each whitespace character becomes an element, in source order") {
    val source = Source(" (\t)\r\n")
    val expected: List[LexicalElement] = List(
      WhitespaceChar.Space, Token.LeftParen,
      WhitespaceChar.Tab, Token.RightParen,
      WhitespaceChar.CarriageReturn, WhitespaceChar.NewLine
    )
    assertEquals(scan(source), expected)
  }

  test("whitespace splits a two-character operator") {
    val source = Source("= =")
    val expected: List[LexicalElement] = List(Token.Equal, WhitespaceChar.Space, Token.Equal)
    assertEquals(scan(source), expected)
  }

  test("a lexeme of one or two characters compiles") {
    assertNoDiff(compileErrors("""Lexeme("=")"""), "")
    assertNoDiff(compileErrors("""Lexeme("==")"""), "")
  }

  test("a lexeme of three characters does not compile") {
    val errors = compileErrors("""Lexeme("...")""")
    // Assert the length check's own message, so an unrelated error can't pass for it.
    assert(
      errors.contains(
        """Cannot prove that scala.compiletime.ops.string.Length[("..." : String)] <= (2 : Int)"""
      ),
      errors
    )
  }

  test("every lexer splits its input losslessly and makes progress") {
    val samples = List("", "(", "()", "==", "=(", "!=!", "<=>", "/", "//", "// hi\n(", " x", "\t\r\n", "@#")
    for
      lexer <- leafLexers
      sample     <- samples
      src         = sample.toList
      (_, consumed, rest) <- lexer.lex(src)
    do
      assertEquals(consumed ++ rest, src, s"lossless split failed on \"$sample\"")
      assert(consumed.nonEmpty, s"no progress on \"$sample\"")
  }

  test("the order of lexer doesn't change what the longest match finds") {
    val samples = List("", "(", "==", "=(", "!=!", "<=>", "/", "//", "// hi\n(", " x", "\t\r\n", "@#")
    for
      sample <- samples
      src     = sample.toList
      order  <- leafLexers.permutations
    do
      assertEquals(
        longest(order).lex(src),
        longest(leafLexers).lex(src),
        s"the order changed the result on \"$sample\""
      )
  }

  test("no two fixed-spelling elements share a spelling, within or across enums") {
    // Two matches of the same length at the same spot have the same spelling, so a duplicate
    // spelling is the only way longest match can tie and become order-dependent.
    val duplicates = fixedSpellings.groupBy(_.spelling).filter(_._2.size > 1)
    assertEquals(duplicates, Map.empty[List[Char], List[FixedSpelling]])
  }

  test("a long source does not overflow the stack") {
    // Plain recursion overflows the JVM stack at ~10,000 characters.
    val tokens = partition(scan(Source("(" * 100_000))).tokens
    assertEquals(tokens.size, 100_000)
  }


// Properties: rules that must hold for every source, checked on generated inputs.
// In this file so they can share `alphabet` with the example-based tests above.
class ScannerProperties extends munit.ScalaCheckSuite:

  import org.scalacheck.Gen
  import org.scalacheck.Prop.forAll

  // Built from fragments, not just characters: drawn character by character, "//" is rare, so
  // comments would almost never be generated and the comment properties would pass vacuously.
  private val fragments: List[String] = alphabet.map(_.toString) :+ Comment.delimiter.mkString
  private val sources: Gen[String] = Gen.listOf(Gen.oneOf(fragments)).map(_.mkString)

  property("round trip: the scanned elements' source texts rebuild the source exactly") {
    forAll(sources) { text =>
      val source = Source(text)
      assertEquals(unscan(scan(source)), source)
    }
  }

  // Sources that start at a line boundary: every variable-length element (today, only Comment)
  // stops at its terminator, and no fixed spelling contains it, so nothing can cross the boundary.
  private val sourcesAfterNewline: Gen[String] = sources.map(s => Comment.terminator.char +: s)

  // Without the newline this is false: "=" ++ "=" scans as EqualEqual, and "// hi" ++ "(" as one comment.
  property("induction: when b starts with a newline, scanning a ++ b equals scanning a, then b") {
    forAll(sources, sourcesAfterNewline) { (a, b) =>
      assertEquals(scan(Source(a ++ b)), scan(Source(a)) ++ scan(Source(b)))
    }
  }

  // Invariants: properties of scan's output alone, whatever the source.

  // Each element next to the one after it, e.g. (LeftParen, Equal), (Equal, NewLine).
  private def neighbours(elements: List[LexicalElement]): List[(LexicalElement, LexicalElement)] =
    elements.zip(elements.drop(1))

  property("invariant: no element has empty source text (every step makes progress)") {
    forAll(sources) { text =>
      scan(Source(text)).foreach(e => assert(e.sourceText.nonEmpty, s"$e has empty source text"))
    }
  }

  property("invariant: a comment's content never contains its terminator") {
    forAll(sources) { text =>
      scan(Source(text)).collect { case c: Comment => c }.foreach { c =>
        assert(!c.content.contains(Comment.terminator.char), s"$c contains its terminator")
      }
    }
  }

  property("invariant: a comment is followed by its terminator, or by nothing") {
    forAll(sources) { text =>
      neighbours(scan(Source(text))).collect { case (c: Comment, next) => (c, next) }.foreach { (c, next) =>
        assertEquals(next, Comment.terminator, s"$c is followed by $next")
      }
    }
  }

  // Maximal munch, seen from its result, e.g. never Equal, Equal. Stated from the spellings and the
  // comment delimiter, not by asking the lexers: a check that asks the code under test agrees with it.
  property("invariant: no element could have been extended into its neighbour (maximal munch)") {
    forAll(sources) { text =>
      neighbours(scan(Source(text))).foreach { (e, next) =>
        val both = (e.sourceText ++ next.sourceText).toList
        val longer = fixedSpellings.filter(f => f.spelling.size > e.sourceText.size && both.startsWith(f.spelling))
        assertEquals(longer, Nil, s"$e could have been extended into $next")
        if both.startsWith(Comment.delimiter) then
          assert(e.isInstanceOf[Comment], s"$e, $next should have started a comment")
      }
    }
  }


