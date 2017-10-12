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

			val correctSequence1 = Seq("WORD", "INT");
			val correctSequence2 = Seq("WORD", "HEXINT");
			val correctSequence3 = Seq("WORD", "ID");

			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence2
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence1
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence3
		}

		else if (category == "JUMP") {
			val correctSequence1 = Seq("ID", "REG");
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence1
		}

		else if (category == "ASSS") {
			val correctSequence1 = Seq("ID", "REG", "COMMA", "REG", "COMMA", "REG")
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

		else if (category == "LMM") { //lis, or mflo, or mfhi
			var correctSequence1 = Seq("ID", "REG");
			seqOfCorrectSeq =  seqOfCorrectSeq :+ correctSequence1;
		}

		else if (category == "MMDD") {
			var correctSequence1 = Seq("ID", "REG", "COMMA", "REG");
			seqOfCorrectSeq =  seqOfCorrectSeq :+ correctSequence1;
		}

		else if (category == "LS") {

			var correctSequence1 = Seq("ID", "REG", "COMMA", "INT", "LPAREN", "REG", "RPAREN");
			var correctSequence2 = Seq("ID", "REG", "COMMA", "HEXINT", "LPAREN", "REG", "RPAREN");

			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence1;
			seqOfCorrectSeq = seqOfCorrectSeq :+ correctSequence2;

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
				seqOfCorrectSeq = seqOfCorrectSeq :+ s;
			}
		}
		
		return seqOfCorrectSeq;
	}


	def getCorrectTokenKindSequences(firstToken: Token) : Seq[Seq[String]] = {
		var category = Utilities.getCategoryOfToken(firstToken);
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

		var seqOfKinds = Seq[String]();

		for (i <- 0 until tokenLine.length) {
			var token = tokenLine.apply(i);
			//we're ignoring the labels at the start of instructions (ie definitions) because there can be arbitrary no. of them
			if (token.kind != "LABEL" || seqOfKinds.length > 0 ) seqOfKinds = appendToSequence(seqOfKinds, token.kind);
		}
		//check if the given sequence of kind of tokens is one of correctTokenSequences
		return correctTokenSequences.contains(seqOfKinds);
	}
}