/***
  * Scanning starter code for CS241 A3
  * This file is where you should implement your solution.
  * Created by Sean Harrap for CS241 in Winter 2017.
  */

import Scanning._

object Asm {
  /* A sequence of sequences of tokens, where each inside sequence represents a line.
   * For example, tokenLines(3)(4) is the fifth token on line 4.
   */
  val tokenLines: Seq[Seq[Token]] = io.Source.stdin.getLines.map(scan).toSeq

  type SymbolTable = Map[String, Int]

  /*
   * This currently just prints tokens one-by-one to standard output.
   * You should replace this with your assembler code.
   */
  def assemble(): Unit = {
    for (tokenLine <- tokenLines) {
      /* For short function calls such as this one, you can use a foreach as well, such as
       * tokenLine.foreach(println)
       */
      for (token <- tokenLine) {
        println(token)
      }
    }
  }

  def main(args: Array[String]): Unit = assemble()
}
