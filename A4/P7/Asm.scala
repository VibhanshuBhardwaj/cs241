import Scanning._
import Analysis._
import Utilities._
import Synthesis._

object Asm {
	/* A sequence of sequences of tokens, where each inside sequence represents a line.
	 * For example, tokenLines(3)(4) is the fifth token on line 4.
	*/
	var tokenLines: Seq[Seq[Token]] = io.Source.stdin.getLines.map(scan).toSeq
	val allInstructionsWithKindID = Seq("jr", "jalr", "add", "sub", "sltu", "slt", "bne", "beq") // PIN
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

		printSymbolTable()

		def firstInstructionType(tokenLine: Seq[Token]) : String  = {
			if (tokenLine.length == 0) return "NONE";
			else if (tokenLine.apply(0).kind == "LABEL") {
				return firstInstructionType(tokenLine.drop(1));
			}
			else return tokenLine.apply(0).lexeme;

		}
		def replaceOperandsInToken(token: Token, instrLex: String, lineNumber: Int) : Token = {
			if (token.kind == "ID") {
				if (!symTable.contains(token.lexeme)) {
					Console.err.println("ERROR");
		 			Console.err.println("The label " + token.lexeme + " was never defined")
		 			System.exit(1);
		 			return token;
				}
				else { //the label exists
					if (instrLex == ".word") {
							return new Token("INT", symTable(token.lexeme).toString)
		 			}
					else if (instrLex == "beq" || instrLex == "bne") {
		 					val labelVal = symTable(token.lexeme).toString.toInt;
		 					val intOffset = (labelVal - (lineNumber*4) - 4) / 4;
		 					//if ()
		 					return new Token ("INT", intOffset.toString);
		 			}
		 			else return token;
				}
			}
			else return token;
		}
		def replaceOperandLabelsWithValue(tokenLine: Seq[Token], lineNumber: Int) : Seq[Token] = {
			if (tokenLine.length == 0) {
				Console.err.println("shouldnt happen because I am calling this on nonNullTokenLines")
				return Seq[Token]();
			}
			else if (tokenLine.apply(0).kind == "LABEL") {
				return replaceOperandLabelsWithValue(tokenLine.drop(1), lineNumber)
			}
			else {
				val firstToken = tokenLine.apply(0); 
				//first token is always an instruction like jr, not something that needs to be replaced
				val instructionLex = firstToken.lexeme;
				val replacedOperands = tokenLine.drop(1).map(x => replaceOperandsInToken(x, instructionLex, lineNumber))
				val ret = firstToken +: replacedOperands;
				return ret;
			}
		}
		//removing empty lines and lines with nothing except label defintions. like a:b:c:
		
		var nonNullTokenLines = tokenLines.filter(x => x.length > 0 && firstInstructionType(x) != "NONE")

		var sanitizedTokenLines = nonNullTokenLines.zipWithIndex.map{ case (tokenLine, index) => replaceOperandLabelsWithValue(tokenLine, index)}

		
		for (tokenLine <- sanitizedTokenLines) { 
			Synthesis.toMachineLanguage(tokenLine);
		}
	}

	def main(args: Array[String]): Unit = {
		val t0 = System.nanoTime();
		assemble()
		val t1 = System.nanoTime();
		//println("time taken is " + (t1 - t0).toString + " nanoseconds")
	}
}
