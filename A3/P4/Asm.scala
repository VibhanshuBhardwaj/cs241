/***
	* This file is where you should implement your solution.
	*/

//TODO: THINK ABOUT CATEGORIES (some instructions are very similar in structure) AND MAYBE RENAMING THEM WITH FAMILY?

import Scanning._

//pass 1
object Analysis {

	def appendToSequence(s: Seq[String], toAdd: String) : Seq[String] = {
		val ret = s :+ toAdd;
		return ret;
	}

	def getAllPossibleCategories() : Seq[String] = {
		var categories = Seq[String]();
		categories = categories :+ "WORD"
		categories = categories :+ "LABEL"
		return categories
	}

	def getTokenSequencesOfCategory(category: String) : Seq[Seq[String]] = {
		var seqOfCorrectSeq = Seq[Seq[String]]()

		if (category == "NOCATEGORY") {
			println("tried to get valid token sequences of NOCATEGORY. Shouldn't happpen")
			return seqOfCorrectSeq;
		}

		else if (category == "WORD") {
			var correctSequence1 = Seq[String]();
			correctSequence1 = appendToSequence(correctSequence1, "WORD");
			correctSequence1 = appendToSequence(correctSequence1, "INT");
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence1

			var correctSequence2 = Seq[String]();
			correctSequence2 = appendToSequence(correctSequence2, "WORD");
			correctSequence2 = appendToSequence(correctSequence2, "HEXINT");
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence2

			var correctSequence3 = Seq[String]();
			correctSequence3 = appendToSequence(correctSequence3, "WORD");

			//for the instruction .word <LABEL_NAME>. The token's kind is ID
			correctSequence3 = appendToSequence(correctSequence3, "ID"); 
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence3
		}


		//for label our correct sequences are intentionally not starting with LABEL itself
		// because a label definition can be followed by an arbitrary number of label definitions
		// eg, a: b: c: d: e: .word $2.
		//so we just don't start our correct sequence with arbitrary no. of labels. we simply ignore and check if
		// what follows label is a correct sequence, and if it is the entire sequence is correct.

		else if (category == "LABEL") { 
			var correctSequence1 = Seq[String]();
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence1;

			var correctSequence2 = Seq[String]();
			val allPossibleCategories : Seq[String] = getAllPossibleCategories();
			var allCorrectSequences : Seq[Seq[String]] = Seq[Seq[String]]();

			//for an instruction starting with LABEL, sequences of correct tokens are all possible correct sequences
			// of all instructions because a label can be followed by any instruction.

			for (c <- allPossibleCategories) {
				//checked for not LABEL to prevent infinite recursion	
				if (c != "LABEL") {
					allCorrectSequences = allCorrectSequences ++ getTokenSequencesOfCategory(c)
				}
			}

			for (s <- allCorrectSequences) {
				//var prependedLabelToCorrectSequence : Seq[String] = "LABEL" +: s
				seqOfCorrectSeq = seqOfCorrectSeq :+ s;
			}
		}
		return seqOfCorrectSeq;
	}

	def getCategoryOfToken(myToken: Token) : String = {

		if (myToken.kind == "WORD") return "WORD";
		else if (myToken.kind == "LABEL") return "LABEL"
		else return "NOCATEGORY"
	}

	def getCorrectTokenKindSequences(firstToken: Token) : Seq[Seq[String]] = {
		var category = getCategoryOfToken(firstToken); // is it one of (add, sub, slt, sltu) or one of other 6 types
		var correctTokenSequences = getTokenSequencesOfCategory(category)
		return correctTokenSequences;
	}

	def isTokenLineCorrect(tokenLine: Seq[Token]) : Boolean  = {
		//check if the line is a correct instruction. if empty, it's correct

		if (tokenLine.length == 0)  {
			return true; //an empty line is correct 
		}
		var firstToken = tokenLine.apply(0);

		var correctTokenSequences = getCorrectTokenKindSequences(firstToken);
		//check if the given sequence of kind of tokens is one of correctTokenSequences

		var seqOfKinds = Seq[String]();

		for (i <- 0 until tokenLine.length) {
			var token = tokenLine.apply(i);
			//we're ignoring the labels at the start of instructions (ie definitions) because there can be arbitrary no. of them
			if (token.kind != "LABEL" || seqOfKinds.length > 0 ) seqOfKinds = appendToSequence(seqOfKinds, token.kind);
		}
		
		return correctTokenSequences.contains(seqOfKinds);
	}
}

//pass 2

object Synthesis {

	def outputByte(i: Int) { //better name?
		var byteArr = Array[Byte]();
		byteArr = byteArr :+ ((i >> 24) & 0xff).toChar.toByte
		byteArr = byteArr :+ ((i >> 16) & 0xff).toChar.toByte
		byteArr = byteArr :+ ((i >> 8) & 0xff).toChar.toByte
		byteArr = byteArr :+ ((i) & 0xff).toChar.toByte
		System.out.write(byteArr)
	}

	//TODO: combine the getCategoryOfToken in Analysis and Synthesis together
	def getCategoryOfToken(myToken: Token) : String = {
		if (myToken.kind == "WORD") return "WORD"
		else if (myToken.kind == "LABEL") return "LABEL"
		else return "NOCATEGORY"
	}

	def printMachineCodeForWORD(tokenLine: Seq[Token]) {
		var value = tokenLine.apply(1).toLong.toInt;
		outputByte(value)
	}

	def printMachineCodeForLABEL(tokenLine: Seq[Token]) {
		toMachineLanguage(tokenLine.drop(1)); //simply print the MC for the instruction after label
	}

	def printMachineCode(category: String, tokenLine: Seq[Token]) {

		if (category == "WORD") {
			printMachineCodeForWORD(tokenLine)	
		}

		else if (category == "LABEL") {
			printMachineCodeForLABEL(tokenLine);
		}
	}

	def toMachineLanguage(tokenLine: Seq[Token]) {
		if (tokenLine.length == 0) return; //an empty line wont ouput anything
		val firstToken = tokenLine.apply(0);
		val category = getCategoryOfToken(firstToken);

		printMachineCode(category, tokenLine);
	}
}

object Asm {
	/* A sequence of sequences of tokens, where each inside sequence represents a line.
	 * For example, tokenLines(3)(4) is the fifth token on line 4.
	*/
	val tokenLines: Seq[Seq[Token]] = io.Source.stdin.getLines.map(scan).toSeq
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
			var labelsReplacedWithValue = tokenLine.map( x=> replaceOperandLabelsWithValue(x))
			sanitizedTokenLines =  sanitizedTokenLines :+ labelsReplacedWithValue
			//we will convert them to binary here.
		}
		for (tokenLine <- sanitizedTokenLines) { 
			Synthesis.toMachineLanguage(tokenLine);
		}
	}

	def main(args: Array[String]): Unit = assemble()
}
