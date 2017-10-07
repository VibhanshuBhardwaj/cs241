import Scanning._

object Analysis {

	def appendToSequence(s: Seq[String], toAdd: String) : Seq[String] = {
		val ret = s :+ toAdd;
		return ret;
	}

	def getAllPossibleFamilies() : Seq[String] = {
		var categories = Seq[String]();
		categories = categories :+ "WORD"
		categories = categories :+ "LABEL"
		return categories
	}

	def getTokenSequencesOfFamily(family: String) : Seq[Seq[String]] = { //correct token sequence of what FOLLOWS the family
		//for eg, for the family JUMP, the only correct sequence is "REG". so we'll output [["REG"]]
		var seqOfCorrectSeq = Seq[Seq[String]]()

		if (family == "NOCATEGORY") {
			println("tried to get valid token sequences of NOCATEGORY. Shouldn't happpen")
			return seqOfCorrectSeq;
		}

		else if (family == "WORD") {
			var correctSequence1 = Seq[String]();
			//correctSequence1 = appendToSequence(correctSequence1, "WORD");
			correctSequence1 = appendToSequence(correctSequence1, "INT");
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence1

			var correctSequence2 = Seq[String]();
			//correctSequence2 = appendToSequence(correctSequence2, "WORD");
			correctSequence2 = appendToSequence(correctSequence2, "HEXINT");
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence2

			var correctSequence3 = Seq[String]();
			//correctSequence3 = appendToSequence(correctSequence3, "WORD");

			//for the instruction .word <LABEL_NAME>. The token's kind is ID
			correctSequence3 = appendToSequence(correctSequence3, "ID"); 
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence3
		}


		//for label our correct sequences are intentionally not starting with LABEL itself
		// because a label definition can be followed by an arbitrary number of label definitions
		// eg, a: b: c: d: e: .word $2.
		//so we just don't start our correct sequence with arbitrary no. of labels. we simply ignore and check if
		// what follows label is a correct sequence, and if it is the entire sequence is correct.

		else if (family == "LABEL") { 
			var correctSequence1 = Seq[String]();
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence1;

			var correctSequence2 = Seq[String]();
			val allPossibleFamilies : Seq[String] = getAllPossibleFamilies();
			var allCorrectSequences : Seq[Seq[String]] = Seq[Seq[String]]();

			//for an instruction starting with LABEL, sequences of correct tokens are all possible correct sequences
			// of all instructions because a label can be followed by any instruction.

			for (c <- allPossibleFamilies) {
				//checked for not LABEL to prevent infinite recursion	
				if (c != "LABEL") {
					allCorrectSequences = allCorrectSequences ++ getTokenSequencesOfFamily(c)
				}
			}

			for (s <- allCorrectSequences) {
				//var prependedLabelToCorrectSequence : Seq[String] = "LABEL" +: s
				seqOfCorrectSeq = seqOfCorrectSeq :+ s;
			}
		}
		return seqOfCorrectSeq;
	}

	def getFamilyOfToken(myToken: Token) : String = {

		if (myToken.kind == "WORD") return "WORD";
		else if (myToken.kind == "LABEL") return "LABEL"
		else return "NOCATEGORY"
	}

	def getCorrectTokenKindSequences(firstToken: Token) : Seq[Seq[String]] = {
		var family = getFamilyOfToken(firstToken); // is it one of (add, sub, slt, sltu) or one of other 6 types
		var correctTokenSequences = getTokenSequencesOfFamily(family)
		return correctTokenSequences;
	}

	def prepend(seq: Seq[String], s: String) : Seq[String] = {
		println("prepend not working")
		var ret = s +: seq;
		return ret;
	}
	def getCompleteCorrectTokenSequence(token: Token) : Seq[Seq[String]] {
		var validTokenSequencesOfFamily = getTokenSequencesOfFamily(getFamilyOfToken(token));
		if (token.kind == "LABEL") return validTokenSequencesOfFamily;
		else {
			var ret = validTokenSequencesOfFamily;
			ret = validTokenSequencesOfFamily.map(x => prepend(x, token.kind));
		}
	}
	def isTokenLineCorrect(tokenLine: Seq[Token]) : Boolean  = {
		//check if the line is a correct instruction. if empty, it's correct

		if (tokenLine.length == 0)  {
			return true; //an empty line is correct 
		}
		var firstToken = tokenLine.apply(0);

		var correctTokenSequences = getCorrectTokenKindSequences(firstToken); //THIS DOES NOT INCLUDE THE KIND OF firstToken.
		println("correctTokenSequences: ")
		correctTokenSequences.map(x => print(x + ", "))
		println("")
		var completeCorrectTokenSequence = correctTokenSequences.map(x => {
			print("firstToken.kind " + firstToken.kind)
			if (firstToken.kind != "LABEL") prepend(x, firstToken.kind);
			else x
		})  //complete cuz it includes the kind of firstToken
		println("completeCorrectTokenSequence: ")
		completeCorrectTokenSequence.map(x => print(x + ", "))
		println("")
		//check if the given sequence of kind of tokens is one of correctTokenSequences

		var seqOfKinds = Seq[String]();
		//constructing the sequence of kinds of token for GIVEN instructions
		for (i <- 0 until tokenLine.length) {
			var token = tokenLine.apply(i);
			//we're ignoring the labels at the start of instructions (ie definitions) because there can be arbitrary no. of them
			if (token.kind != "LABEL" || seqOfKinds.length > 0 ) seqOfKinds = appendToSequence(seqOfKinds, token.kind);
		}
		println("seqOfKinds: ")
		seqOfKinds.map(x => print(x + ", "))
		println("")
		return correctTokenSequences.contains(seqOfKinds);
	}
}