import Scanning._
import Analysis._
import Utilities._
import Synthesis._

object Asm {

	var tokenLines: Seq[Seq[Token]] = io.Source.stdin.getLines.map(scan).toSeq
	var symTable = collection.mutable.Map[String, Int]()

	def printSymbolTable() {
		for ((k, v) <- symTable) {
			Console.err.println(k + " " + v)
		}
	}

	def processLabels(tokenLine: Seq[Token], programCounter: Int) : Int = {

		val len = tokenLine.length;
		if (len > 0) {
			var firstToken = tokenLine.apply(0);

			if (firstToken.kind == "LABEL") {
				var lex = firstToken.lexeme;
				var id = lex.take(lex.length - 1)
				if (symTable.contains(id)) {
					Console.err.println("ERROR")
					Console.err.println("REDIFINING A LABEL IS NOT ALLOWED. You have multiple definitions of " + firstToken.lexeme);

					System.exit(1);
				}
				else {
					var lex = firstToken.lexeme;
					symTable = symTable + (lex.take(lex.length - 1) -> programCounter)
					if (len > 1) processLabels(tokenLine.drop(1), programCounter);
				}
			}
			else {
				return programCounter + 4;
			}	
		}
		return programCounter;
	}

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
			val instructionLex = firstToken.lexeme;

			//first token is always an instruction like jr (cuz we've removed label defintions in recursive call)
			//it's not something that needs to be replaced so we will drop it and replace in the rest of sequence
			
			val replacedOperands = tokenLine.drop(1).map(x => replaceOperandsInToken(x, instructionLex, lineNumber))
			val ret = firstToken +: replacedOperands;
			return ret;
		}
	}

	def assemble(): Unit = {

		var programCounter = 0;
		for (tokenLine <- tokenLines) {
			val isTokenLineValid = Analysis.isTokenLineCorrect(tokenLine);

			if (!isTokenLineValid) {
				Console.err.println("ERROR")
				Console.err.println("Token sequence invalid for instruction " + tokenLine.toString);
				System.exit(1);;
			}
			programCounter = processLabels(tokenLine, programCounter);
		}

		printSymbolTable()

		//removing empty lines and lines with nothing except label defintions. like a:b:c:		
		var nonNullTokenLines = tokenLines.filter(x => x.length > 0 && firstInstructionType(x) != "NONE")
		var sanitizedTokenLines = nonNullTokenLines.zipWithIndex.map{ case (tokenLine, index) => replaceOperandLabelsWithValue(tokenLine, index)}

		for (tokenLine <- sanitizedTokenLines) { 
			Synthesis.toMachineLanguage(tokenLine);
		}
	}

	def main(args: Array[String]): Unit = {

		val t0 = System.nanoTime();

		assemble(); //tada!

		val t1 = System.nanoTime();
		//println("time taken is " + (t1 - t0).toString + " nanoseconds")
	}
}
