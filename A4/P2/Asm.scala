/***
	* This file is where you should implement your solution.
	*/

//TODO: THINK ABOUT CATEGORIES (some instructions are very similar in structure) AND MAYBE RENAMING THEM WITH FAMILY?

import Scanning._
import Analysis._
import Utilities._
import Synthesis._

object Asm {
	/* A sequence of sequences of tokens, where each inside sequence represents a line.
	 * For example, tokenLines(3)(4) is the fifth token on line 4.
	*/
	val tokenLines: Seq[Seq[Token]] = io.Source.stdin.getLines.map(scan).toSeq
	val allInstructionsWithKindID = Seq("jr", "jalr", "add", "sub", "sltu", "slt") // PIN
	def printSymbolTable() {
		for ((k, v) <- symTable) {
			Console.err.println(k + " " + v)
		}
	}

	var symTable = collection.mutable.Map[String, Int]()

	/*
	 * This currently just prints tokens one-by-one to standard output.
	 * You should replace this with your assembler code.
	*/

	def assemble(): Unit = {
		var ProgramCounter = 0;
		for (tokenLine <- tokenLines) {
		/* For short function calls such as this one, you can use a foreach as well, such as
		 * tokenLine.foreach(println)
		*/
			//println("my tokenLine " + tokenLine);
			val isTokenLineValid = Analysis.isTokenLineCorrect(tokenLine);
			//println("is my tokenLine valid? " + isTokenLineValid)

			if (!isTokenLineValid) {
				Console.err.println("ERROR")
				Console.err.println("Token sequence invalid for instruction " + tokenLine.toString);
				return;
			}

			def processLine(tokenLine: Seq[Token]) {
				val len = tokenLine.length;
				if (len > 0) {
					var firstToken = tokenLine.apply(0);

					if (firstToken.kind == "LABEL") {
						var lex = firstToken.lexeme;
						var id = lex.take(lex.length - 1)
						//println("checking if " + id + " exists in symTable")
						if (symTable.contains(id)) {
							Console.err.println("ERROR")
							Console.err.println("REDIFINING A LABEL IS NOT ALLOWED. You have multiple definitions of " + firstToken.lexeme);
							//return; //exit here!!!!
							System.exit(1);
						}
						else {
							var lex = firstToken.lexeme;
							symTable = symTable + (lex.take(lex.length - 1) -> ProgramCounter)
							if (len > 1) processLine(tokenLine.drop(1));
						}
					}
					else {
						ProgramCounter = ProgramCounter + 4;
					}	
				}
			}

			processLine(tokenLine);
		}

		//printSymbolTable()
		def replaceOperandLabelsWithValue(myToken: Token) : Token = {
			if (allInstructionsWithKindID.contains(myToken.lexeme)) return myToken; //ie an instruction like jr, not a label that needs to be replaced
			if (myToken.kind == "ID") {
				if (!symTable.contains(myToken.lexeme)) {
					Console.err.println("ERROR");
					Console.err.println("The label " + myToken.lexeme + " was never defined")
					System.exit(1);
					return myToken;
				}
				else {
					return new Token("INT", symTable(myToken.lexeme).toString)
				}
			}
			else return myToken;
		}
		
		var sanitizedTokenLines = Seq[Seq[Token]]();
		for (tokenLine <- tokenLines) {
			//this only happens when all instructions are determined to be correct.
			var labelsReplacedWithValue = tokenLine.map( x=> replaceOperandLabelsWithValue(x)) // PIN 
			
			sanitizedTokenLines = sanitizedTokenLines :+ labelsReplacedWithValue
		}
		for (tokenLine <- sanitizedTokenLines) { 
			Synthesis.toMachineLanguage(tokenLine);
		}
	}

	def main(args: Array[String]): Unit = assemble()
}
