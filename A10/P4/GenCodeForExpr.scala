import gen.Node;

import MIPSOutput._;
import GenCodeForTerm._;
import TypeChecker._;
import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.Map;

object GenCodeForExpr {
	
	var FINALSYMTABLE =  ArrayBuffer[FunctionSymTable]();
	type FunctionSymTable = (String, Map[String, String], Int);
	def init(st: ArrayBuffer[FunctionSymTable]) {
		FINALSYMTABLE = st;
	}

	def generate(expr: Node) : Unit = {
		val children = expr.children;

		if (expr.rule == "expr term") {
			//println("for term called with " + children(0).value)
			GenCodeForTerm.generate(children(0));
		}

		else if (expr.rule == "expr expr PLUS term") {
			val exp2Type = TypeChecker.checkTypes(children(0), FINALSYMTABLE, "wain");
			val termType = TypeChecker.checkTypes(children(2), FINALSYMTABLE, "wain");
			//println("; exp2 Type is " + exp2Type + " termType is " + termType);
			MIPSOutput.append("; expr -> expr PLUS term");
			if (exp2Type == "int" && termType == "int") {
				generate(children(0));

				var push3Inst = "sw $3, -4($30)"
				var extendStackInst = "sub $30, $30, $4"

				MIPSOutput.append(push3Inst);
				MIPSOutput.append(extendStackInst);

				GenCodeForTerm.generate(children(2));

				var reduceStackInst = "add $30, $30, $4";
				var pop5Inst = "lw $5, -4($30)";

				MIPSOutput.append(reduceStackInst);
				MIPSOutput.append(pop5Inst);
				var finalAddInst = "add $3, $5, $3";
				MIPSOutput.append(finalAddInst);
			}
			else if (exp2Type == "int*" && termType == "int") {
				generate(children(0));

				var push3Inst = "sw $3, -4($30)"
				var extendStackInst = "sub $30, $30, $4"

				MIPSOutput.append(push3Inst);
				MIPSOutput.append(extendStackInst);

				GenCodeForTerm.generate(children(2));
				MIPSOutput.append("mult $3, $4");
				MIPSOutput.append("mflo $3");
				var reduceStackInst = "add $30, $30, $4";
				var pop5Inst = "lw $5, -4($30)";

				MIPSOutput.append(reduceStackInst);
				MIPSOutput.append(pop5Inst);
				var finalAddInst = "add $3, $5, $3";
				MIPSOutput.append(finalAddInst);
			}
			else if (exp2Type == "int" && termType == "int*") {
				generate(children(0));
				MIPSOutput.append("mult $3, $4");
				MIPSOutput.append("mflo $3");
				var push3Inst = "sw $3, -4($30)"
				var extendStackInst = "sub $30, $30, $4"

				MIPSOutput.append(push3Inst);
				MIPSOutput.append(extendStackInst);

				GenCodeForTerm.generate(children(2));
				var reduceStackInst = "add $30, $30, $4";
				var pop5Inst = "lw $5, -4($30)";

				MIPSOutput.append(reduceStackInst);
				MIPSOutput.append(pop5Inst);
				var finalAddInst = "add $3, $5, $3";
				MIPSOutput.append(finalAddInst);
			}

		}

		else if (expr.rule == "expr expr MINUS term") {

			val exp2Type = TypeChecker.checkTypes(children(0), FINALSYMTABLE, "wain");
			val termType = TypeChecker.checkTypes(children(2), FINALSYMTABLE, "wain");
			if (exp2Type == "int" && termType == "int") {
				MIPSOutput.append("; expr -> expr MINUS term")
				generate(children(0));

				var push3Inst = "sw $3, -4($30)"
				var extendStackInst = "sub $30, $30, $4"

				MIPSOutput.append(push3Inst);
				MIPSOutput.append(extendStackInst);

				GenCodeForTerm.generate(children(2));

				var reduceStackInst = "add $30, $30, $4";
				var pop5Inst = "lw $5, -4($30)";

				MIPSOutput.append(reduceStackInst);
				MIPSOutput.append(pop5Inst);
				var finalAddInst = "sub $3, $5, $3";
				MIPSOutput.append(finalAddInst);
			}
			else if (exp2Type == "int*" && termType == "int") {
				generate(children(0));

				var push3Inst = "sw $3, -4($30)"
				var extendStackInst = "sub $30, $30, $4"

				MIPSOutput.append(push3Inst);
				MIPSOutput.append(extendStackInst);

				GenCodeForTerm.generate(children(2));
				MIPSOutput.append("mult $3, $4");
				MIPSOutput.append("mflo $3");

				var reduceStackInst = "add $30, $30, $4";
				var pop5Inst = "lw $5, -4($30)";

				MIPSOutput.append(reduceStackInst);
				MIPSOutput.append(pop5Inst);
				var finalAddInst = "sub $3, $5, $3";
				MIPSOutput.append(finalAddInst);
			}
			else if (exp2Type == "int*" && termType == "int*") {
				
				generate(children(0));

				var push3Inst = "sw $3, -4($30)"
				var extendStackInst = "sub $30, $30, $4"

				MIPSOutput.append(push3Inst);
				MIPSOutput.append(extendStackInst);

				GenCodeForTerm.generate(children(2));

				var reduceStackInst = "add $30, $30, $4";
				var pop5Inst = "lw $5, -4($30)";

				MIPSOutput.append(reduceStackInst);
				MIPSOutput.append(pop5Inst);
				var finalAddInst = "sub $3, $5, $3";
				MIPSOutput.append(finalAddInst);
				MIPSOutput.append("div $3, $4");
				MIPSOutput.append("mflo $3");
			}
		}
	}
}