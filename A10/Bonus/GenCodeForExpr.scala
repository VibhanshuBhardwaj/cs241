import gen.Node;

import MIPSOutput._;
import GenCodeForTerm._;
import TypeChecker._;
import Utils._
import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.Map;

object GenCodeForExpr {
	
	var FINALSYMTABLE =  ArrayBuffer[FunctionSymTable]();
	type FunctionSymTable = (String, Map[String, String], Int);
	def init(st: ArrayBuffer[FunctionSymTable]) {
		FINALSYMTABLE = st;
	}
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
	def generate(expr: Node, funcName:String) : Unit = {
		val children = expr.children;

		if (expr.rule == "expr term") {
			if (isConstantExpr(expr)) {
				MIPSOutput.append("lis $3");
				val num = getNumTerm(children(0));
				MIPSOutput.append(".word " + num);
			}
			//println("for term called with " + children(0).value)
			else {GenCodeForTerm.generate(children(0), funcName);}
		}

		else if (expr.rule == "expr expr PLUS term") {
			val exp2Type = TypeChecker.checkTypes(children(0), FINALSYMTABLE, funcName);
			val termType = TypeChecker.checkTypes(children(2), FINALSYMTABLE, funcName);
			//println("; exp2 Type is " + exp2Type + " termType is " + termType);
			MIPSOutput.append("; expr -> expr PLUS term");
			if (exp2Type == "int" && termType == "int") {
				if (isConstantExpr(children(0)) && isConstantTerm(children(2))) {
					val result = getNumExpr(children(0)).toInt + getNumTerm(children(2)).toInt;
					MIPSOutput.append("lis $3");
					MIPSOutput.append(".word " + result);
				}
				else  {
					generate(children(0), funcName);

					Utils.push(3);

					GenCodeForTerm.generate(children(2), funcName);

					Utils.pop(5);

					var finalAddInst = "add $3, $5, $3";
					MIPSOutput.append(finalAddInst);
				}
			}
			else if (exp2Type == "int*" && termType == "int") {
				generate(children(0), funcName);

				Utils.push(3);

				GenCodeForTerm.generate(children(2), funcName);
				MIPSOutput.append("mult $3, $4");
				MIPSOutput.append("mflo $3");

				Utils.pop(5);

				var finalAddInst = "add $3, $5, $3";
				MIPSOutput.append(finalAddInst);
			}
			else if (exp2Type == "int" && termType == "int*") {
				generate(children(0), funcName);
				MIPSOutput.append("mult $3, $4");
				MIPSOutput.append("mflo $3");
				Utils.push(3);

				GenCodeForTerm.generate(children(2), funcName);
				Utils.pop(5);
				var finalAddInst = "add $3, $5, $3";
				MIPSOutput.append(finalAddInst);
			}

		}

		else if (expr.rule == "expr expr MINUS term") {

			val exp2Type = TypeChecker.checkTypes(children(0), FINALSYMTABLE, funcName);
			val termType = TypeChecker.checkTypes(children(2), FINALSYMTABLE, funcName);
			if (exp2Type == "int" && termType == "int") {
				if (isConstantExpr(children(0)) && isConstantTerm(children(2))) {
					val result = getNumExpr(children(0)).toInt - getNumTerm(children(2)).toInt;
					MIPSOutput.append("lis $3");
					MIPSOutput.append(".word " + result);
				}
				else { 
					MIPSOutput.append("; expr -> expr MINUS term")
					generate(children(0), funcName);

					Utils.push(3)

					GenCodeForTerm.generate(children(2), funcName);

					Utils.pop(5);

					var finalAddInst = "sub $3, $5, $3";
					MIPSOutput.append(finalAddInst);
				}
			}
			else if (exp2Type == "int*" && termType == "int") {
				generate(children(0), funcName);

				Utils.push(3);

				GenCodeForTerm.generate(children(2), funcName);
				MIPSOutput.append("mult $3, $4");
				MIPSOutput.append("mflo $3");

				Utils.pop(5);
				var finalAddInst = "sub $3, $5, $3";
				MIPSOutput.append(finalAddInst);
			}
			else if (exp2Type == "int*" && termType == "int*") {
				
				generate(children(0), funcName);

				Utils.push(3)

				GenCodeForTerm.generate(children(2), funcName);

				Utils.pop(5);
				var finalAddInst = "sub $3, $5, $3";
				MIPSOutput.append(finalAddInst);
				MIPSOutput.append("div $3, $4");
				MIPSOutput.append("mflo $3");
			}
		}
	}
}