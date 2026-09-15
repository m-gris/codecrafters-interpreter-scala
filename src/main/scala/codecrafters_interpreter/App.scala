package codecrafters_interpreter


import java.io.IOException
import java.nio.file.{Files, Path}

object Main {

  def main(args: Array[String]): Unit = {
    println("EOF  null")
    // HUGLY & BRITTLE ... but work for now.
    val filename = args(1)
    val fileContent: String = Files.readString(Path.of(filename))
    val rez: List[Token] = scan(Source(fileContent))
    rez.map(_.render).foreach(println)
  }

}
