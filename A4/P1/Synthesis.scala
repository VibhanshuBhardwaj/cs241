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

	def printMachineCodeForWORD(tokenLine: Seq[Token]) {
		var value = tokenLine.apply(1).toLong.toInt;
		outputByte(value)
	}

	def printMachineCodeForLABEL(tokenLine: Seq[Token]) {
		toMachineLanguage(tokenLine.drop(1)); //simply print the MC for the instruction after label
	}

	def printMachineCodeForJUMP(tokenLine: Seq[Token]) { //PIN
		val firstToken = tokenLine.apply(0);
		val lex = firstToken.lexeme;
		val valueOfRegister = tokenLine.apply(1).toLong.toInt;
		if (lex == "jr") {
			outputByte((valueOfRegister << 21) | 8); 
		}
		else if (lex == "jalr") {
			outputByte((valueOfRegister << 21) | 9);
		}
	}
	def printMachineCode(category: String, tokenLine: Seq[Token]) {

		if (category == "WORD") {
			printMachineCodeForWORD(tokenLine)	
		}

		else if (category == "LABEL") {
			printMachineCodeForLABEL(tokenLine);
		}
		else if (category == "JUMP") {
			printMachineCodeForJUMP(tokenLine);
		}
	}

	def toMachineLanguage(tokenLine: Seq[Token]) {
		if (tokenLine.length == 0) return; //an empty line wont ouput anything
		val firstToken = tokenLine.apply(0);
		val category = Utilities.getCategoryOfToken(firstToken);

		printMachineCode(category, tokenLine);
	}
}