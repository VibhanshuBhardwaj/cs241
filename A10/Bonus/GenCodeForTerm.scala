import gen.Node;

import MIPSOutput._;
import GenCodeForFactor._;
import Utils._;

object GenCodeForTerm {
	def isConstantExpr(expr: Node) : Boolean = {
		if (expr.rule == "expr term") {
			val term = expr.children(0)
			if (term.rule == "term factor") {
				val fct = term.children(0);
				if (fct.rule == "factor NUM") {
					return true;
				}
				else return false;
			}
			else return false;
		}
		else return false;
	}
	def isConstantFactor(fct: Node) : Boolean = {
		if (fct.rule == "factor NUM") {
			return true;
		}
		else if (fct.rule == "factor LPAREN expr RPAREN") return isConstantExpr(fct.children(1));
		else return false;
	}
	def isConstantTerm(term: Node) : Boolean = {
		if (term.rule == "term factor") {
			val fct = term.children(0);
			if (fct.rule == "factor NUM") {
				return true;
			}
			else return false;
		}
		else return false;
	}
	def getNumExpr(expr: Node) : String = {
		return expr.children(0).children(0).children(0).lex;
	}
	def getNumTerm(term: Node) : String = {
		return term.children(0).children(0).lex;
	}
	def getNumFct(fct: Node) : String = {
		if (fct.rule == "factor NUM") {
			return fct.children(0).lex;
		}
		else  {
			return getNumExpr(fct.children(1));
		}
		//else return "NAN";
	}



	def generate(term: Node, funcName: String) : Unit = {
		val children = term.children;
		val rule = term.rule;
		if (rule == "term factor") {
			//println("for factor called with " + children(0).value)
			GenCodeForFactor.generate(children(0), funcName);
		}

		else if (rule == "term term STAR factor") {
			if (isConstantTerm(children(0)) && isConstantFactor(children(2))) {
				val result = getNumTerm(children(0)).toInt * getNumFct(children(2)).toInt;
				MIPSOutput.append("lis $3");
				MIPSOutput.append(".word " + result);
			}
			else  {
				MIPSOutput.append("; term -> term STAR factor code starts");
				generate(children(0), funcName);
				Utils.push(3);

				GenCodeForFactor.generate(children(2), funcName);
				Utils.pop(5);

				val multiplyThem = "mult $3, $5";
				val mflo = "mflo $3";
				MIPSOutput.append(multiplyThem);
				MIPSOutput.append(mflo) 
				MIPSOutput.append("; term -> term STAR factor code ends")
			}

		}
		else if (rule == "term term SLASH factor") {
			if (isConstantTerm(children(0)) && isConstantFactor(children(2))) {
				val result = getNumTerm(children(0)).toInt / getNumFct(children(2)).toInt;
				MIPSOutput.append("lis $3");
				MIPSOutput.append(".word " + result);
			}
			else {
				MIPSOutput.append("; term -> term SLASH factor code starts");
				generate(children(0), funcName);
				Utils.push(3)

				GenCodeForFactor.generate(children(2), funcName);
				Utils.pop(5)
				val divThem = "div $5, $3";
				val mflo = "mflo $3";
				MIPSOutput.append(divThem);
				MIPSOutput.append(mflo) 
				MIPSOutput.append("; term -> term SLASH factor code ends")
			}
		}
		else if (rule == "term term PCT factor") {
			if (isConstantTerm(children(0)) && isConstantFactor(children(2))) {
				val result = getNumTerm(children(0)).toInt % getNumFct(children(2)).toInt;
				MIPSOutput.append("lis $3");
				MIPSOutput.append(".word " + result);
			}
			else {
				MIPSOutput.append("; term -> term PCT factor code starts");
				generate(children(0), funcName);
				Utils.push(3)

				GenCodeForFactor.generate(children(2), funcName);
				Utils.pop(5)
				val divThem = "div $5, $3";
				val mflo = "mfhi $3";
				MIPSOutput.append(divThem);
				MIPSOutput.append(mflo) 
				MIPSOutput.append("; term -> term PCT factor code ends")
			}

		}
	}
}