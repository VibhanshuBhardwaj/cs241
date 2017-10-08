import Scanning._
import Utilities._

object Analysis {

	def appendToSequence(s: Seq[String], toAdd: String) : Seq[String] = {
		val ret = s :+ toAdd;
		return ret;
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

		else if (category == "JUMP") {
			var correctSequence1 = Seq[String]();
			correctSequence1 = appendToSequence(correctSequence1, "ID")
			correctSequence1 = appendToSequence(correctSequence1, "REG")
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence1
		}
		else if (category == "ASSS") {
			var correctSequence1 = Seq[String]();
			correctSequence1 = appendToSequence(correctSequence1, "ID")
			correctSequence1 = appendToSequence(correctSequence1, "REG")
			correctSequence1 = appendToSequence(correctSequence1, "COMMA")
			correctSequence1 = appendToSequence(correctSequence1, "REG")
			correctSequence1 = appendToSequence(correctSequence1, "COMMA")
			correctSequence1 = appendToSequence(correctSequence1, "REG")
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence1
		}
		else if (category == "BRANCH") {
			var correctSequence1 = Seq("ID", "REG", "COMMA", "REG", "COMMA", "INT");
			var correctSequence2 = Seq("ID", "REG", "COMMA", "REG", "COMMA", "HEXINT");
			var correctSequence3 = Seq("ID", "REG", "COMMA", "REG", "COMMA", "ID");
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence1;
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence2;
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence3;
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
			val allPossibleCategories : Seq[String] = Utilities.getAllPossibleCategories();
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


	def getCorrectTokenKindSequences(firstToken: Token) : Seq[Seq[String]] = {
		var category = Utilities.getCategoryOfToken(firstToken); // is it one of (add, sub, slt, sltu) or one of other 6 types
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
		//println("correctTokenSequences: ")
		//correctTokenSequences.map(x=> print( x + ", "))
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