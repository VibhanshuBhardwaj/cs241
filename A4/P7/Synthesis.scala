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

	def processOffset(offsetToken: Token) : Int = {
		val kindOfOffset = offsetToken.kind;
		var offset = offsetToken.toLong.toInt;
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
		offset = (offset & 0xffff)
		return offset;
	}

	def printMachineCodeForBRANCH(sanitizedTokenLine: Seq[Token]) {

		val firstToken = sanitizedTokenLine.apply(0);
		val lex = firstToken.lexeme;
		val registerS = sanitizedTokenLine.apply(1).toLong.toInt;
		val registerT = sanitizedTokenLine.apply(3).toLong.toInt;
		
		val offset = processOffset(sanitizedTokenLine.apply(5))

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

	def printMachineCodeForMMDD(sanitizedTokenLine: Seq[Token]) {

		val lex = sanitizedTokenLine.apply(0).lexeme;
		val registerS = sanitizedTokenLine.apply(1).toLong.toInt;
		val registerT = sanitizedTokenLine.apply(3).toLong.toInt;
		var instruction = 0;
		instruction  = (registerS << 21) | (registerT << 16) | instruction;
		
		if (lex == "mult") {
			instruction = instruction | 24;
		}
		else if (lex == "multu") {
			instruction = instruction | 25;
		}
		else if (lex == "div") {
			instruction = instruction | 26;
		}
		else if (lex == "divu") {
			instruction = instruction | 27;
		}
		outputByte(instruction);
	}

	def printMachineCodeForLS(sanitizedTokenLine: Seq[Token]) {

		val lex = sanitizedTokenLine.apply(0).lexeme;
		val registerT = sanitizedTokenLine.apply(1).toLong.toInt;
		val kindOfOffset = sanitizedTokenLine.apply(3).kind;
		val offset = processOffset(sanitizedTokenLine.apply(3));
		val registerS = sanitizedTokenLine.apply(5).toLong.toInt;


		if (lex == "lw") {
			outputByte(0x8c000000|(registerS<<21)|(registerT<<16)|offset)
		}
		else if (lex =="sw") {
			outputByte(0xac000000|(registerS<<21)|(registerT<<16)|offset)
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
		else if (category == "MMDD") {
			printMachineCodeForMMDD(sanitizedTokenLine);
		}
		else if (category == "LS") {
			printMachineCodeForLS(sanitizedTokenLine)
		}
	}

	def toMachineLanguage(sanitizedTokenLine: Seq[Token]) {

		if (sanitizedTokenLine.length == 0) return; //an empty line wont output anything
		val firstToken = sanitizedTokenLine.apply(0);
		val category = Utilities.getCategoryOfToken(firstToken);

		printMachineCode(category, sanitizedTokenLine);
	}
}