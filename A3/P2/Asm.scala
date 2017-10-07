/***
	* Scanning starter code for CS241 A3
	* This file is where you should implement your solution.
	* Created by Sean Harrap for CS241 in Winter 2017.
	*/

import Scanning._

//pass 1
object Analysis {

	def appendToSequence(s: Seq[String], toAdd: String) : Seq[String] = {
		val ret = s :+ toAdd;
		return ret;
	}

	def getTokenSequencesOfCategory(category: String) : Seq[Seq[String]] = { //return sequence of string  or token?
		var seqOfCorrectSeq = Seq[Seq[String]]()
		if (category == "NOCATEGORY") return seqOfCorrectSeq;

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

		return seqOfCorrectSeq;
	}

	def getCategoryOfToken(myToken: Token) : String = {
		if (myToken.kind == "WORD") return "WORD";
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
		else return "NOCATEGORY"
	}
	def printMachineCodeForWORD(tokenLine: Seq[Token]) {
		var value = tokenLine.apply(1).toLong.toInt;
		//println("lexeme valueInLong " + valueInLong);
		//println("incoming int " + value)
		output(value)
		//print(value)
	}
	def printMachineCode(category: String, tokenLine: Seq[Token]) {
		if (category == "WORD") {
			printMachineCodeForWORD(tokenLine)
			
		}
	}
	def toMachineLanguage(tokenLine: Seq[Token]) {
		if (tokenLine.length == 0) return; //an empty line wont ouput anything
		val firstToken = tokenLine.apply(0);
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
			//println("my tokenLine " + tokenLine);
			val isTokenLineValid = Analysis.isTokenLineCorrect(tokenLine);
			if (!isTokenLineValid) {
				Console.err.println("ERROR")
				Console.err.println("Token sequence invalid for instruction " + tokenLine.toString);
				return;
			}
			for (token <- tokenLine) {
				//println("my token" + token)
			}
		}

		for (tokenLine <- tokenLines) {
			//this only happens when all instructions are determined to be correct.
			//we will convert them to binary here.
			Synthesis.toMachineLanguage(tokenLine);

		}
	}

	def main(args: Array[String]): Unit = assemble()
}
