package codecrafters_interpreter

import java.io.IOException
import java.nio.file.{Files, Path}


enum TokenType:
  case LeftParen, RightParen, EndOfFile

case class Token(tokenType: TokenType, sourceFragment: String)

def scan(source: String): List[Token] = ???


object Main {

  def main(args: Array[String]): Unit = {

  }

}
