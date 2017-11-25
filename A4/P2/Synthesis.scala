import Scanning._
import Utilities._

object Synthesis {

	def outputByte(i: Int) { //better name?
		var byteArr = Array[Byte]();
		byteArr = byteArr :+ ((i >> 24) & 0xff).toChar.toByte
		byteArr = byteArr :+ ((i >> 16) & 0xff).toChar.toByte
		byteArr = byteArr :+ ((i >> 8) & 0xff).toChar.toByte
		byteArr = byteArr :+ ((i) & 0xff).toChar.toByte
		System.out.write(byteArr)
	}

	def printMachineCodeForWORD(sanitizedTokenLine: Seq[Token]) {
		var value = sanitizedTokenLine.apply(1).toLong.toInt;
		outputByte(value)
	}

	def printMachineCodeForLABEL(sanitizedTokenLine: Seq[Token]) {
		toMachineLanguage(sanitizedTokenLine.drop(1)); //simply print the MC for the instruction after label
	}

	def printMachineCodeForJUMP(sanitizedTokenLine: Seq[Token]) { //PIN
		val firstToken = sanitizedTokenLine.apply(0);
		val lex = firstToken.lexeme;
		val valueOfRegister = sanitizedTokenLine.apply(1).toLong.toInt;
		if (lex == "jr") {
			outputByte((valueOfRegister << 21) | 8); 
		}
		else if (lex == "jalr") {
			outputByte((valueOfRegister << 21) | 9);
		}
	}
	def printMachineCodeForASSS(sanitizedTokenLine: Seq[Token]) {
		val firstToken = sanitizedTokenLine.apply(0);
		val lex = firstToken.lexeme;
		val registerD = sanitizedTokenLine.apply(1).toLong.toInt;
		val registerS = sanitizedTokenLine.apply(3).toLong.toInt;
		val registerT = sanitizedTokenLine.apply(5).toLong.toInt;
		var intToPrint = 0;
		intToPrint = (registerD << 11) | intToPrint;
		intToPrint = (registerT << 16) | intToPrint;
		intToPrint = (registerS << 21) | intToPrint;
		if (lex == "add") {
			intToPrint = intToPrint | 32;
		}
		else if (lex == "sub") {
			intToPrint = intToPrint | 34;
		}
		else if (lex =="slt") {
			intToPrint = intToPrint | 42;
		}
		else if (lex == "sltu") {
			intToPrint = intToPrint | 43;
		}
		outputByte(intToPrint);

	}
	def printMachineCode(category: String, sanitizedTokenLine: Seq[Token]) {

		if (category == "WORD") {
			printMachineCodeForWORD(sanitizedTokenLine)	
		}

		else if (category == "LABEL") {
			printMachineCodeForLABEL(sanitizedTokenLine);
		}
		else if (category == "JUMP") {
			printMachineCodeForJUMP(sanitizedTokenLine);
		}
		else if (category == "ASSS") {
			printMachineCodeForASSS(sanitizedTokenLine);
		}
	}

	def toMachineLanguage(sanitizedTokenLine: Seq[Token]) {
		if (sanitizedTokenLine.length == 0) return; //an empty line wont ouput anything
		val firstToken = sanitizedTokenLine.apply(0);
		val category = Utilities.getCategoryOfToken(firstToken);

		printMachineCode(category, sanitizedTokenLine);
	}
}