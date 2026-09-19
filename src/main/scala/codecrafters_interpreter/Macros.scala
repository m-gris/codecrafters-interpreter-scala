package codecrafters_interpreter

import scala.quoted.*

// --- debugging helper -------------------------------------------------------
// Prints what the compiler knows about a type, at compile time.
// Allows to explore the reflection API.
// Usage: dbg[Token] anywhere in code, then compile and read the [info] output.

inline def dbg[T]: Unit = ${ dbgImpl[T] }

private def dbgImpl[T: Type](using Quotes): Expr[Unit] =
  import quotes.reflect.*
  val tpe = TypeRepr.of[T]
  val sym = tpe.typeSymbol
  report.info(
    s"""|type     : ${tpe.show}
        |symbol   : ${sym.name}
        |flags    : ${sym.flags.show}
        |children : ${sym.children.map(_.name)}""".stripMargin
  )
  '{ () }

// --- checked ScanError construction ----------------------------------------
// ScanError(c) stays available for runtime use, where c comes from a file and
// nothing can be checked. ScanError.checked(c) is for *literal* arguments, and
// refuses characters that some Token already claims.

object ScanErrorMacro:

  inline def checked(inline c: Char): ScanError = ${ checkedImpl('c) }

  private def checkedImpl(cExpr: Expr[Char])(using Quotes): Expr[ScanError] =
    import quotes.reflect.*
    cExpr.value match
      case None =>
        // not a literal: nothing to check, emit the plain construction
        '{ ScanError($cExpr) }
      case Some(c) =>

        val legalChars: Set[Char] = Token.values.collect {
          case t if t.lexeme.nonEmpty => t.lexeme.head
         }.toSet

        if legalChars.contains(c) then
          report.errorAndAbort(s"'$c' is a legal token character, not a scan error")
        else
          '{ ScanError(${ Expr(c) }) }

