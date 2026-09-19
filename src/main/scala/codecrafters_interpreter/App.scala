package codecrafters_interpreter


import java.io.IOException
import java.nio.file.{Files, Path}

object Main {

  def main(args: Array[String]): Unit = {
    // HUGLY & BRITTLE ... but work for now.
    val filename = args(1)
    val fileContent: String = Files.readString(Path.of(filename))
    val rez: ScanResult = scan(Source(fileContent))
    rez.tokens.map(_.render).foreach(println)
  }

}
