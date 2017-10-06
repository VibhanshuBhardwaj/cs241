/***
	* Scanning starter code for CS241 A3
	* This file is where you should implement your solution.
	* Created by Sean Harrap for CS241 in Winter 2017.
	*/

import Scanning._

//pass 1
object Analysis {

	//type SymbolTable = Map[String, Int]

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
	def getTokenSequencesOfCategory(category: String) : Seq[Seq[String]] = { //return sequence of string  or token?
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
		}
		else if (category == "LABEL") {
			var correctSequence1 = Seq[String]();
			correctSequence1 = appendToSequence(correctSequence1, "LABEL");
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence1;

			var correctSequence2 = Seq[String]();
			correctSequence2 = appendToSequence(correctSequence2, "LABEL");
			val allPossibleCategories : Seq[String] = getAllPossibleCategories();
			var allCorrectSequences : Seq[Seq[String]] = Seq[Seq[String]]();
			//for LABEL, sequences of correct tokens are all
			for (c <- allPossibleCategories) {
				//checked for not LABEL to prevent infinite recursion	
				if (c != "LABEL") {
					allCorrectSequences = allCorrectSequences ++ getTokenSequencesOfCategory(c)
				}
			}
			for (s <- allCorrectSequences) {
				var prependedLabelToCorrectSequence : Seq[String] = "LABEL" +: s
				seqOfCorrectSeq = seqOfCorrectSeq :+ prependedLabelToCorrectSequence;
			}

		}

		return seqOfCorrectSeq;
	}

	def getCategoryOfToken(myToken: Token) : String = {
		//println("getting category of token " + myToken.kind)
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
		//return true; //for now

		if (tokenLine.length == 0)  {
			return true; //an empty line is correct 
		}
		var firstToken = tokenLine.apply(0);

		var correctTokenSequences = getCorrectTokenKindSequences(firstToken);
		//check if sequnece of kind of tokens is one of correctTokenSequences
		var seqOfKinds = Seq[String]();
		for (token <- tokenLine) {
			seqOfKinds = appendToSequence(seqOfKinds, token.kind);
		}
		
		//println("is it correct syntactically? " + correctTokenSequences.contains(seqOfKinds));
		return correctTokenSequences.contains(seqOfKinds);
	}
}

//pass 2

object Synthesis {
	def output(i: Int) { //better name?
		var byteArr = Array[Byte]();
		byteArr = byteArr :+ ((i >> 24) & 0xff).toChar.toByte
		byteArr = byteArr :+ ((i >> 16) & 0xff).toChar.toByte
		byteArr = byteArr :+ ((i >> 8) & 0xff).toChar.toByte
		byteArr = byteArr :+ ((i) & 0xff).toChar.toByte
		System.out.write(byteArr)
		//print(( (i >> 24) & 0xff).toChar);
		//print(( (i >> 16) & 0xff).toChar);
		//print(( (i >> 8) & 0xff).toChar);
		//print(( i & 0xff).toChar);
		// System.out.print((i >> 24).toChar);
		// System.out.print((i >> 16).toChar);
		// System.out.print((i >> 8).toChar);
		// System.out.print((i).toChar);

	}
	def getCategoryOfToken(myToken: Token) : String = {
		if (myToken.kind == "WORD") return "WORD"
		else if (myToken.kind == "LABEL") return "LABEL"
		else return "NOCATEGORY"
	}
	def printMachineCodeForWORD(tokenLine: Seq[Token]) {
		var value = tokenLine.apply(1).toLong.toInt;
		//println("lexeme valueInLong " + valueInLong);
		//println("incoming int " + value)
		output(value)
		//print(value)
	}
	def printMachineCodeForLABEL(tokenLine: Seq[Token]) {
		toMachineLanguage(tokenLine.drop(1)); //simply print the MC for the instruction after label
	}
	def printMachineCode(category: String, tokenLine: Seq[Token]) {
		//println("here, category is " + category)
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
		//println("firstToken " + firstToken.kind + " " + firstToken.lexeme)
		val category = getCategoryOfToken(firstToken);

		printMachineCode(category, tokenLine);
		//println("here goes the final machine code output")

	}
}

object Asm {
	/* A sequence of sequences of tokens, where each inside sequence represents a line.
	 * For example, tokenLines(3)(4) is the fifth token on line 4.
	*/
	val tokenLines: Seq[Seq[Token]] = io.Source.stdin.getLines.map(scan).toSeq
	def printSymbolTable() {
		for ((k, v) <- symTable) {
			Console.err.println(k.take(k.length - 1) + " " + v)
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
			
			val len = tokenLine.length;

			if (!isTokenLineValid) {
				Console.err.println("ERROR")
				Console.err.println("Token sequence invalid for instruction " + tokenLine.toString);
				return;
			}

			def processLine(tokenLine: Seq[Token]) {
				if (len > 0) {
					var firstToken = tokenLine.apply(0);
			//	println("firstToken.kind is " + firstToken.kind)
					if (firstToken.kind == "LABEL") {
						if (symTable.contains(firstToken.lexeme)) {
							Console.err.println("ERROR")
							Console.err.println("REDIFINING A LABEL IS NOT ALLOWED. You have multiple definitions of " + firstToken.lexeme);
							return;
						}
						else {
							symTable = symTable + (firstToken.lexeme -> ProgramCounter)
							if (len > 1) processLine(tokenLine.drop(1));
						}
					}
					else {
				//	println("encountered just insturction no label. updating PC from " + ProgramCounter)
						ProgramCounter = ProgramCounter + 4;
				//	println(" updated to " + ProgramCounter)
					}	
				}
			}
			processLine(tokenLine);
			//for (token <- tokenLine) {
				//println("my token" + token)
			//}
		}
		printSymbolTable()
		for (tokenLine <- tokenLines) {
			//this only happens when all instructions are determined to be correct.
			//we will convert them to binary here.
			Synthesis.toMachineLanguage(tokenLine);

		}
	}

	def main(args: Array[String]): Unit = assemble()
}
