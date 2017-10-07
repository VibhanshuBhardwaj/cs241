import Scanning._

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