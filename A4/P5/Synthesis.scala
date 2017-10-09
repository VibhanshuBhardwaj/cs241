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

	def printMachineCodeForJUMP(sanitizedTokenLine: Seq[Token]) {
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
	def printMachineCodeForBRANCH(sanitizedTokenLine: Seq[Token]) {
		//print it here
		val firstToken = sanitizedTokenLine.apply(0);
		val lex = firstToken.lexeme;
		val registerS = sanitizedTokenLine.apply(1).toLong.toInt;
		val registerT = sanitizedTokenLine.apply(3).toLong.toInt;
		val kindOfOffset = sanitizedTokenLine.apply(5).kind;
		var offset = sanitizedTokenLine.apply(5).toLong.toInt; //only works for hex or int now
		//println("offset " + sanitizedTokenLine.apply(5).toLong.toInt)
		if (kindOfOffset != "HEXINT" && (offset > 32767 || offset < -32768)) {
			Console.err.println("ERROR");
			Console.err.println("Offset " + offset + " outside the range of 16 bit 2's complement")
			System.exit(1);
		}
		else if (kindOfOffset == "HEXINT") {
			if (offset > 65535 || offset < 0) {
				Console.err.println("ERROR");
				Console.err.println("Offset " + offset + " outside the range of possible hex values")
				System.exit(1);
			}
		}
		offset = (offset & 0xffff) //covert to 16 bit 2's complement
		var intToPrint = 0;
		intToPrint = (registerS << 21) | (registerT << 16) | offset | intToPrint;
		if (lex == "beq") {
			intToPrint = intToPrint | (4 << 26);
		}
		else if (lex == "bne") {
			intToPrint = intToPrint | (5 << 26);
		}
		outputByte(intToPrint);
	}
	def printMachineCodeForLMM(sanitizedTokenLine: Seq[Token]) {
		val lex = sanitizedTokenLine.apply(0).lexeme;
		val register = sanitizedTokenLine.apply(1).toLong.toInt;
		if (lex == "lis") {
			outputByte( (register << 11) | 20 )
		}
		else if (lex == "mfhi") {
			outputByte( (register << 11) | 16 )
		}
		else if (lex == "mflo") {
			outputByte( (register << 11) | 18 )
		}
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
		else if (category == "BRANCH") {
			printMachineCodeForBRANCH(sanitizedTokenLine);
		}
		else if (category == "LMM") {
			printMachineCodeForLMM(sanitizedTokenLine);
		}
	}

	def toMachineLanguage(sanitizedTokenLine: Seq[Token]) {
		if (sanitizedTokenLine.length == 0) return; //an empty line wont ouput anything
		val firstToken = sanitizedTokenLine.apply(0);
		val category = Utilities.getCategoryOfToken(firstToken);

		printMachineCode(category, sanitizedTokenLine);
	}
}