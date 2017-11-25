import gen.Node;

import MIPSOutput._;
import GenCodeForTerm._;

object GenCodeForExpr {
	def generate(expr: Node) : Unit = {
		val children = expr.children;

		if (expr.rule == "expr term") {
			//println("for term called with " + children(0).value)
			GenCodeForTerm.generate(children(0));
		}

		else if (expr.rule == "expr expr PLUS term") {
			MIPSOutput.append("; expr -> expr PLUS term");
			generateCodeForExpr(children(0));

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

		else if (expr.rule == "expr expr MINUS term") {
			MIPSOutput.append("; expr -> expr MINUS term")
			generateCodeForExpr(children(0));

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
	}
}