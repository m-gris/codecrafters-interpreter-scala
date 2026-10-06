package codecrafters_interpreter


private val leafLexers: List[Lexer[Recognized]] =
  List(summon[Lexer[Comment]], summon[Lexer[Punctuator]], summon[Lexer[WhitespaceChar]])

// Every value with a fixed spelling. A new FixedSpelling enum must be added here.
private val fixedSpellings: List[FixedSpelling] =
  Punctuator.values.toList ++ WhitespaceChar.values.toList

// The characters generated inputs are drawn from: every character some lexer knows (taken from
// the spellings, so new fixed spellings are included automatically; this also covers `//` via Slash),
// plus characters no lexer knows, so unrecognised input and its boundaries get exercised too.
private val alphabet: List[Char] =
  fixedSpellings.flatMap(_.spelling.characters).distinct ++ List('@', 'a')

class ScannerSuite extends munit.FunSuite:

  // Printed by munit in place of "values are not the same", so a failure shows its input.
  private def clue[A](
      source: Source,
      expected: A,
      actual: A
  ): String =
    s"""
       |  source   = "$source"
       |  expected = $expected
       |  obtained = $actual""".stripMargin

  test("empty source yields an empty list") {
    val emptySource = Source("")
    val expected: List[LexicalElement] = Nil
    val actual = scan(emptySource)
    assertEquals(actual, expected, clue(emptySource, expected, actual))
  }

  test("two single-char tokens side by side are both kept") {
    val source = Source("()")
    val expected: List[LexicalElement] = List(Punctuator.LeftParen, Punctuator.RightParen)
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("a multi-token source yields its tokens in source order") {
    val source = Source("())")
    val expected: List[LexicalElement] = List(Punctuator.LeftParen, Punctuator.RightParen, Punctuator.RightParen)
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("an unrecognised character becomes an UnRecognized element") {
    val source = Source("@")
    val expected: List[LexicalElement] = List(UnRecognized('@'))
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("a token after an unrecognised character is still scanned") {
    val source = Source("@(")
    val expected: List[LexicalElement] = List(UnRecognized('@'), Punctuator.LeftParen)
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("locate tracks line numbers") {
    val source = Source("@(\n\n@")
    val expected =  List(Located(UnRecognized('@'), 1), Located(Punctuator.LeftParen, 1), Located(WhitespaceChar.NewLine, 1), Located(WhitespaceChar.NewLine, 2), Located(UnRecognized('@'), 3))
    val actual = locate(scan(source))
    assertEquals(actual, expected, clue(source, expected, actual))
  }


  test("EqualEqual is recognized") {
    val source = Source("==")
    val expected: List[LexicalElement] = List(Punctuator.EqualEqual)
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }
  test("Equal is recognized") {
    val source = Source("=")
    val expected: List[LexicalElement] = List(Punctuator.Equal)
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("the longest lexeme wins: === is EqualEqual then Equal") {
    val source = Source("===")
    val expected: List[LexicalElement] = List(Punctuator.EqualEqual, Punctuator.Equal)
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("the longest lexeme wins: !!=== is Bang, BangEqual, EqualEqual") {
    val source = Source("!!===")
    val expected: List[LexicalElement] = List(Punctuator.Bang, Punctuator.BangEqual, Punctuator.EqualEqual)
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("the longest lexeme wins: <<=>>= is Less, LessEqual, Greater, GreaterEqual") {
    val source = Source("<<=>>=")
    val expected: List[LexicalElement] = List(Punctuator.Less, Punctuator.LessEqual, Punctuator.Greater, Punctuator.GreaterEqual)
    val actual = scan(source)
    assertEquals(actual, expected, clue(source, expected, actual))
  }

  test("a comment runs to the end of the line, whatever it contains") {
    val source = Source("(///Unicode:£§᯽☺♣)")
    val expected: List[LexicalElement] = List(Punctuator.LeftParen, Comment("/Unicode:£§᯽☺♣)"))
    assertEquals(scan(source), expected)
  }

  test("scanning resumes at the newline that ends a comment") {
    val source = Source("// hi\n(")
    val expected: List[LexicalElement] = List(Comment(" hi"), WhitespaceChar.NewLine, Punctuator.LeftParen)
    assertEquals(scan(source), expected)
  }

  test("each whitespace character becomes an element, in source order") {
    val source = Source(" (\t)\r\n")
    val expected: List[LexicalElement] = List(
      WhitespaceChar.Space, Punctuator.LeftParen,
      WhitespaceChar.Tab, Punctuator.RightParen,
      WhitespaceChar.CarriageReturn, WhitespaceChar.NewLine
    )
    assertEquals(scan(source), expected)
  }

  test("whitespace splits a two-character operator") {
    val source = Source("= =")
    val expected: List[LexicalElement] = List(Punctuator.Equal, WhitespaceChar.Space, Punctuator.Equal)
    assertEquals(scan(source), expected)
  }

  test("a spelling of one or two characters compiles") {
    assertNoDiff(compileErrors("""Spelling("=")"""), "")
    assertNoDiff(compileErrors("""Spelling("==")"""), "")
  }

  test("a spelling of three characters does not compile") {
    val errors = compileErrors("""Spelling("...")""")
    // Assert the length check's own message, so an unrelated error can't pass for it.
    assert(
      errors.contains(
        """Cannot prove that scala.compiletime.ops.string.Length[("..." : String)] <= (2 : Int)"""
      ),
      errors
    )
  }

  test("an empty spelling does not compile") {
    val errors = compileErrors("""Spelling("")""")
    // Assert the length check's own message, so an unrelated error can't pass for it.
    assert(
      errors.contains(
        """Cannot prove that scala.compiletime.ops.string.Length[("" : String)] >= (1 : Int)"""
      ),
      errors
    )
  }

  test("every lexer splits its input losslessly, makes progress, and its element matches its lexeme") {
    val samples = List("", "(", "()", "==", "=(", "!=!", "<=>", "/", "//", "// hi\n(", " x", "\t\r\n", "@#")
    for
      lexer <- leafLexers
      sample     <- samples
      src         = sample.toList
      (element, lexeme, rest) <- lexer.lex(src)
    do
      assertEquals(lexeme ++ rest, src, s"lossless split failed on \"$sample\"")
      assert(lexeme.nonEmpty, s"no progress on \"$sample\"")
      assertEquals(element.sourceText, lexeme.mkString, s"element and lexeme disagree on \"$sample\"")
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
    // Two fixed spellings matching at the same spot with the same length are the same spelling,
    // so a duplicate is the only way two of them can tie and make longest match order-dependent.
    // A tie with a variable-length lexer (e.g. a punctuator `//` against an empty comment) is
    // caught by the lexer-order test instead.
    val duplicates = fixedSpellings.groupBy(_.spelling).filter(_._2.size > 1)
    assertEquals(duplicates, Map.empty[Spelling, List[FixedSpelling]])
  }

  test("a fixed-spelling element's source text is its spelling") {
    // Guards against storing the source text separately again, where the two could disagree.
    fixedSpellings.foreach(f => assertEquals(f.sourceText, f.spelling.text, s"$f"))
  }

  test("a long source does not overflow the stack") {
    // Plain recursion overflows the JVM stack at ~10,000 characters.
    val elements = scan(Source("(" * 100_000))
    assertEquals(elements.size, 100_000)
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

  // Progress itself is checked per lexer above: a step that consumed nothing would make scan loop
  // forever, so this property could never get to report it.
  property("invariant: no element has empty source text") {
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
        val longer = fixedSpellings.filter(f => f.spelling.characters.size > e.sourceText.size && both.startsWith(f.spelling.characters))
        assertEquals(longer, Nil, s"$e could have been extended into $next")
        if both.startsWith(Comment.delimiter) then
          assert(e.isInstanceOf[Comment], s"$e, $next should have started a comment")
      }
    }
  }


