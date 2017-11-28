import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map; //[String, Int]()

import gen.Node;
import MIPSOutput._;

object Utils {
	var FINALSYMTABLE = ArrayBuffer[FunctionSymTable]();

	type FunctionSymTable = (String, Map[String, String], Int);


	def getValOfLexFromSymTable(lex: String, funcName: String) : String = {
		for (f <- FINALSYMTABLE) {
			if (f._1 == funcName) {
				return f._2(lex);
			}
		}
		println(";SHOULD NOT BE HAPPENING YET");
		return "";
	}
	def push(r: Int) {
		var reg = "$" + r.toString;
		var inst = "sw " + reg + ", " + "-4($30)";
		MIPSOutput.append(inst);
		MIPSOutput.append("sub $30, $30, $4");
	}
	def pop(r: Int) {
		var reg = "$" + r.toString;
		var inst = "lw " + reg + ", " + "-4($30)";
		MIPSOutput.append("add $30, $30, $4");
		MIPSOutput.append(inst);
	}
	def init(table: ArrayBuffer[FunctionSymTable]) {
		FINALSYMTABLE =table;
	}

	def isConstantExpr(expr: Node) : Boolean = {
		val children = expr.children;
		if (expr.rule == "expr term") {
			val term = children(0);
			return isConstantTerm(term);
		}
		if ((expr.rule == "expr expr PLUS term") || (expr.rule == "expr expr MINUS term")) {
			return (isConstantExpr(children(0)) && isConstantTerm(children(2)))
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
		val children = term.children;
		if (term.rule == "term factor") {
			val fct = children(0);
			return isConstantFactor(fct);
		}
		else {
			val term =children(0);
			val fct = children(2);
			return (isConstantTerm(term) && isConstantFactor(fct));
		}
		
	}
	def getNumExpr(expr: Node) : Int = {
		val rule = expr.rule;
		val children = expr.children;
		if (rule == "expr term") return getNumTerm(children(0));
		else if (rule == "expr expr PLUS term") {
			return getNumExpr(children(0)) + getNumTerm(children(2));
		}
		else {
			return getNumExpr(children(0)) - getNumTerm(children(2));
		}
	}
	def getNumTerm(term: Node) : Int = {
		val rule = term.rule;
		val children = term.children;
		if (rule == "term factor") return getNumFct(children(0));
		else if (rule == "term term STAR factor") {
			return getNumTerm(children(0)) * getNumFct(children(2));
		}
		else if (rule == "term term SLASH factor") {
			return getNumTerm(children(0)) / getNumFct(children(2));
		}
		else  {
			return getNumTerm(children(0)) % getNumFct(children(2));
		}
	}
	def getNumFct(fct: Node) : Int = {
		if (fct.rule == "factor NUM") {
			return fct.children(0).lex.toInt;
		}
		else  {
			return getNumExpr(fct.children(1));
		}
		//else return "NAN";
	}
}