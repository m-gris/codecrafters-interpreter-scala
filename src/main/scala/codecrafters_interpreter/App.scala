package codecrafters_interpreter

import java.nio.file.{Files, Path}

object Main {

  def main(args: Array[String]): Unit = {

    args match
      case Array("tokenize", filename) =>

        val fileContent: String = Files.readString(Path.of(filename))
        val ils: List[LexicalElement] = scan(Source(fileContent))
        val rez: ScanResult = partition(ils)
        rez.tokens.render.foreach(println)
        rez.errors.map(_.render).foreach(Console.err.println)
        if rez.errors.isEmpty then sys.exit(0) else sys.exit(65)

      case _ =>
        Console.err.println("Usage: ./your_program.sh tokenize <filename>")
        sys.exit(64)

  }

}
