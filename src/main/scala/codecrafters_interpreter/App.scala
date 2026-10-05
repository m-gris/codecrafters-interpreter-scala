package codecrafters_interpreter

import java.nio.file.{Files, Path}

object Main {

  def main(args: Array[String]): Unit = {

    args match

      case Array("tokenize", filename) =>

        val fileContent: String = Files.readString(Path.of(filename))

        val elmts: List[Located[LexicalElement]] = locate(scan(Source(fileContent)))

        stdoutLines(elmts).foreach(println)

        val errs = stderrLines(elmts)
        errs.foreach(Console.err.println)

        if  errs.isEmpty then sys.exit(0) else sys.exit(65)

      case _ =>
        Console.err.println("Usage: ./your_program.sh tokenize <filename>")
        sys.exit(64)

  }

}
