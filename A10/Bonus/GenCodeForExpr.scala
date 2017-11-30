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

	def generate(expr: Node, funcName:String, regSet: Set[String]) : String = {
		val children = expr.children;
		if (isConstantExpr(expr, funcName)) {
			var r = "";

			if (regSet.size == 0) r = "3"
			else r = regSet.head;

			val result = getNumExpr(expr, funcName);
			if (result == 1) {
				MIPSOutput.append("add $" + r + ", $0, $11");
			}
			else if (result == 0) {
				MIPSOutput.append("add $" + r+ ", $0, $0");
			}
			else if (result == 4) {
				MIPSOutput.append("add $" + r + ", $0, $4");
			}
			else {
				MIPSOutput.append("lis $" + r);
				MIPSOutput.append(".word " + result.toString);
			}
			return r;
		}
		else if (expr.rule == "expr term") {
			MIPSOutput.append(";in expr -> term ")
			val r = GenCodeForTerm.generate(children(0), funcName, regSet);
			MIPSOutput.append("; expr -> term in r " + r)
			return r;
		}

		else if (expr.rule == "expr expr PLUS term") {
			val exp2Type = TypeChecker.checkTypes(children(0), FINALSYMTABLE, funcName);
			val termType = TypeChecker.checkTypes(children(2), FINALSYMTABLE, funcName);
			//println("; exp2 Type is " + exp2Type + " termType is " + termType);
			MIPSOutput.append("; expr -> expr PLUS term");
			if (exp2Type == "int" && termType == "int") {
				val s = generate(children(0), funcName, regSet);
				if (s == "3") {
					Utils.push(3);

					var t = GenCodeForTerm.generate(children(2), funcName, regSet);
					println("; t should be 3 here. it is " + t)

					Utils.pop(5);

					var finalAddInst = "add $3, $5, $3";
					MIPSOutput.append(finalAddInst);
					return "3";
				}
				else {
					var newSet = regSet - s;
					var t = GenCodeForTerm.generate(children(2), funcName, newSet);
					var inst = "add $" + s + ", $" + s + ", $" + t;
					MIPSOutput.append(inst);
					return s;
				}
				
			}
			else if (exp2Type == "int*" && termType == "int") {
				val s = generate(children(0), funcName, regSet);
				if (s == "3") {
					Utils.push(3);

					val t= GenCodeForTerm.generate(children(2), funcName, regSet);
					println("; t should be 3 here. it is " + t)
					MIPSOutput.append("mult $3, $4");
					MIPSOutput.append("mflo $3");

					Utils.pop(5);

					var finalAddInst = "add $3, $5, $3";
					MIPSOutput.append(finalAddInst);
					return "3";
				}
				else {
					var newSet = regSet - s;
					val t = GenCodeForTerm.generate(children(2), funcName, newSet);
					MIPSOutput.append("mult $" + t + ", $4");
					MIPSOutput.append("mflo $" + t);
					MIPSOutput.append("add $" + s + ", $" + s + ", $" + t);
					return s;
				}
			}
			else if (exp2Type == "int" && termType == "int*") {
				val s = generate(children(0), funcName, regSet);
				if (s == "3") {
					MIPSOutput.append("mult $3, $4");
					MIPSOutput.append("mflo $3");
					Utils.push(3);

					val t = GenCodeForTerm.generate(children(2), funcName, regSet);
					println("; t should be 3 here. it is " + t)
					Utils.pop(5);
					var finalAddInst = "add $3, $5, $3";
					MIPSOutput.append(finalAddInst);
					return "3";
				}
				else {
					var newSet = regSet -s;
					MIPSOutput.append("mult $" + s +  ", $4");
					MIPSOutput.append("mflo $" + s);

					val t = GenCodeForTerm.generate(children(2), funcName, newSet);
					MIPSOutput.append("add $" + s + ", $" + s + ", $" + t);
					return s;

				}
			}

		}

		else if (expr.rule == "expr expr MINUS term") {

			val exp2Type = TypeChecker.checkTypes(children(0), FINALSYMTABLE, funcName);
			val termType = TypeChecker.checkTypes(children(2), FINALSYMTABLE, funcName);
			if (exp2Type == "int" && termType == "int") {
				MIPSOutput.append("; expr -> expr MINUS term")
				val s = generate(children(0), funcName, regSet);
				if (s == "3")  {

					Utils.push(3)

					GenCodeForTerm.generate(children(2), funcName, regSet);

					Utils.pop(5);

					var finalAddInst = "sub $3, $5, $3";
					MIPSOutput.append(finalAddInst);
					return "3";
				}
				else {
					var newSet = regSet - s;
					val t = GenCodeForTerm.generate(children(2), funcName, newSet);
					MIPSOutput.append("sub $" + s + ", $" + s + ", $" + t);
					return s;

				}
			}
			else if (exp2Type == "int*" && termType == "int") {
				val s = generate(children(0), funcName, regSet);
				MIPSOutput.append(";s is " + s);
				if (s == "3") {

					Utils.push(3);

					GenCodeForTerm.generate(children(2), funcName, regSet);
					MIPSOutput.append("mult $3, $4");
					MIPSOutput.append("mflo $3");

					Utils.pop(5);
					var finalAddInst = "sub $3, $5, $3";
					MIPSOutput.append(finalAddInst);
					return "3"
				}
				else { 
					MIPSOutput.append("; ptr - int!")
					var newSet = regSet - s;
					val t = GenCodeForTerm.generate(children(2), funcName, newSet);
					MIPSOutput.append("; t is " + t)
					MIPSOutput.append("mult $" + t + ", $4");
					MIPSOutput.append("mflo $" + t);
					MIPSOutput.append("sub $" + s + ", $" + s + ", $" + t);
					return s;

				}
			}
			else if (exp2Type == "int*" && termType == "int*") {
				MIPSOutput.append("; ptr subtraction!")
				val s = generate(children(0), funcName, regSet);
				MIPSOutput.append("; s is " + s);
				if (s == "3") {

					Utils.push(3)

					GenCodeForTerm.generate(children(2), funcName, regSet);

					Utils.pop(5);
					var finalAddInst = "sub $3, $5, $3";
					MIPSOutput.append(finalAddInst);
					MIPSOutput.append("div $3, $4");
					MIPSOutput.append("mflo $3");
					return "3"
				}
				else {
					val newSet = regSet - s;
					val t = GenCodeForTerm.generate(children(2), funcName, newSet);
					MIPSOutput.append("; t is " + t)
					MIPSOutput.append("sub $" + s +", $" + s + ", $" + t);
					MIPSOutput.append("div $" + s + ", $4");
					MIPSOutput.append("mflo $" + s);
					return s;
				}
			}
		}
		return "3";
	}
}