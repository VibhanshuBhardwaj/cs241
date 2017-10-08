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

		//printSymbolTable()
		def replaceOperandLabelsWithValue(myToken: Token, instrLex: String, lineNumber: Int) : Token = {
			//if (allInstructionsWithKindID.contains(myToken.lexeme)) return myToken; //ie an instruction like jr, not a label that needs to be replaced
			if (myToken.kind == "ID") {
				if (!symTable.contains(myToken.lexeme)) {
					Console.err.println("ERROR");
					Console.err.println("The label " + myToken.lexeme + " was never defined")
					System.exit(1);
					return myToken;
				}
				else { //the label exists
						if (instrLex == "word") {
							return new Token("INT", symTable(myToken.lexeme).toString)
						}
						else if (instrLex == "beq" || instrLex == "bne") {
							val labelVal = symTable(myToken.lexeme).toString.toInt;
							val intOffset = (labelVal - (lineNumber*4) - 4) / 4;
							//if ()
							return new Token ("INT", intOffset.toString);
						}
						else return myToken;
				}
			}
			else return myToken;
		}
		
		var sanitizedTokenLines = Seq[Seq[Token]]();
		def firstInstructionType(tokenLine: Seq[Token]) : String  = {
			if (tokenLine.length == 0) return "NONE";
			else if (tokenLine.apply(0).kind == "LABEL") {
				return firstInstructionType(tokenLine.drop(1));
			}
			else return tokenLine.apply(0).lexeme;

		}
		def getOnlyInstructions(tokenLines: Seq[Seq[Token]]) : Seq[Seq[Token]] = {
			var ret = Seq[Seq[Token]]();
			for (tokenLine <- tokenLines) {
			//	println("tokenLine: " + tokenLine + " len: " + tokenLine.length)
				val firstInstr = firstInstructionType(tokenLine)
			//	println("firstInstr " + firstInstr);
				if (tokenLine.length > 0 && firstInstr != "NONE") {
					ret = ret :+ tokenLine
				}
			}
			return ret;
			//println("tokenLines " + tokenLines)
			//tokenLines.map((x: Seq[Token]) => println("bool:  "  + x + " "  + (x.length > 0 && firstInstructionType(x) != "LABEL")))
			//return tokenLines.filter( (x: Seq[Token]) => (x.length > 0 && firstInstructionType(x) != "LABEL"));
			
		}
		tokenLines = getOnlyInstructions(tokenLines)//.filter(_.length > 0)
		//println("tokenLines after only instructions: " + tokenLines.map(print))
		for (i <- 0 until tokenLines.length) {
			//this only happens when all instructions are determined to be correct.
			var tokenLine = tokenLines.apply(i);
			//println("tokenLine " + tokenLine)
			val firstType = firstInstructionType(tokenLine)
			var labelsReplacedWithValue = Seq[Token]();
			//var labelsReplacedWithValue = tokenLine.map( x=> replaceOperandLabelsWithValue(x, firstType, i)) // PIN 
			var encounteredInstruction = false;
			for (token <- tokenLine) {
				var newToken = token;
				if (encounteredInstruction) {
					newToken = replaceOperandLabelsWithValue(token, firstType, i)
				}
				if (token.kind != "LABEL") {
					encounteredInstruction = true;
				}
				labelsReplacedWithValue = labelsReplacedWithValue :+ newToken

			}
			//println("labelsReplacedWithValue " + labelsReplacedWithValue)
			sanitizedTokenLines = sanitizedTokenLines :+ labelsReplacedWithValue
		}
		//println("sanitizedTokenLines " + sanitizedTokenLines)
		for (tokenLine <- sanitizedTokenLines) { 
			Synthesis.toMachineLanguage(tokenLine);
		}
	}

	def main(args: Array[String]): Unit = assemble()
}
