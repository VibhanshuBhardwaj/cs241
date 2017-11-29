import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map; //[String, Int]()

import gen.Node;
import MIPSOutput._;

object Utils {
	var FINALSYMTABLE = ArrayBuffer[FunctionSymTable]();
	var constMappping = Map[String, String]();
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
	def init(table: ArrayBuffer[FunctionSymTable], cm: Map[String, String]) {
		FINALSYMTABLE =table;
		constMappping = cm;
	}

	def isConstantExpr(expr: Node, funcName: String) : Boolean = {
		val children = expr.children;
		if (expr.rule == "expr term") {
			val term = children(0);
			return isConstantTerm(term, funcName);
		}
		if ((expr.rule == "expr expr PLUS term") || (expr.rule == "expr expr MINUS term")) {
			return (isConstantExpr(children(0), funcName) && isConstantTerm(children(2), funcName))
		}
		else return false;
	}
	def isConstantFactor(fct: Node, funcName: String) : Boolean = {
		if (fct.rule == "factor NUM") {
			return true;
		}
		else if (fct.rule == "factor ID") {
			val fullName = funcName + " " + fct.children(0).lex;
			//println("; fullName " + fullName)
			return (constMappping contains fullName);
		}
		else if (fct.rule == "factor LPAREN expr RPAREN") return isConstantExpr(fct.children(1), funcName);
		else return false;
	}
	def isConstantTerm(term: Node, funcName: String) : Boolean = {
		val children = term.children;
		if (term.rule == "term factor") {
			val fct = children(0);
			return isConstantFactor(fct, funcName);
		}
		else {
			val term =children(0);
			val fct = children(2);
			return (isConstantTerm(term, funcName) && isConstantFactor(fct, funcName));
		}
		
	}
	def getNumExpr(expr: Node, funcName: String) : Int = {
		val rule = expr.rule;
		val children = expr.children;
		if (rule == "expr term") return getNumTerm(children(0), funcName);
		else if (rule == "expr expr PLUS term") {
			return getNumExpr(children(0), funcName) + getNumTerm(children(2), funcName);
		}
		else {
			return getNumExpr(children(0), funcName) - getNumTerm(children(2), funcName);
		}
	}
	def getNumTerm(term: Node, funcName: String) : Int = {
		val rule = term.rule;
		val children = term.children;
		if (rule == "term factor") return getNumFct(children(0), funcName);
		else if (rule == "term term STAR factor") {
			return getNumTerm(children(0), funcName) * getNumFct(children(2), funcName);
		}
		else if (rule == "term term SLASH factor") {
			return getNumTerm(children(0), funcName) / getNumFct(children(2), funcName);
		}
		else  {
			return getNumTerm(children(0), funcName) % getNumFct(children(2), funcName);
		}
	}
	def getNumFct(fct: Node, funcName: String) : Int = {
		if (fct.rule == "factor NUM") {
			return fct.children(0).lex.toInt;
		}
		else if (fct.rule == "factor ID") {
			val lex= fct.children(0).lex;
			val fullName = funcName + " " + lex;
			return constMappping(fullName).toInt;
		}
		else  {
			return getNumExpr(fct.children(1), funcName);
		}
		//else return "NAN";
	}
}