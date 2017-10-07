import Scanning._
object Utilities {

	def getCategoryOfToken(myToken: Token) : String = {
			if (myToken.kind == "WORD") return "WORD";
			else if (myToken.kind == "LABEL") return "LABEL"
			else if (myToken.kind == "ID") { 
				val lex = myToken.lexeme;
				if (lex == "jr" || (lex =="jalr")) return "JUMP"
				//add more cases here
				else return "NOCATEGORY"
			}
			else return "NOCATEGORY"
	}
	
	def getAllPossibleCategories() : Seq[String] = {
		var categories = Seq[String]();
		categories = categories :+ "WORD"
		categories = categories :+ "LABEL"
		categories = categories :+ "JUMP"
		return categories
	}
}