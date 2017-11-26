import gen.Node;

import MIPSOutput._;
import GenCodeForFactor._;

object GenCodeForTerm {
	
	def generate(term: Node, funcName: String) : Unit = {
		val children = term.children;
		val rule = term.rule;
		if (rule == "term factor") {
			//println("for factor called with " + children(0).value)
			GenCodeForFactor.generate(children(0), funcName);
		}
		else if (rule == "term term STAR factor") {
			MIPSOutput.append("; term -> term STAR factor code starts");
			generate(children(0), funcName);
			var push3Inst = "sw $3, -4($30)"
			var extendStackInst = "sub $30, $30, $4"

			MIPSOutput.append(push3Inst);
			MIPSOutput.append(extendStackInst);

			GenCodeForFactor.generate(children(2), funcName);
			var reduceStackInst = "add $30, $30, $4";
			var pop5Inst = "lw $5, -4($30)";

			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append(pop5Inst);
			val multiplyThem = "mult $3, $5";
			val mflo = "mflo $3";
			MIPSOutput.append(multiplyThem);
			MIPSOutput.append(mflo) 
			MIPSOutput.append("; term -> term STAR factor code ends")


		}
		else if (rule == "term term SLASH factor") {
			MIPSOutput.append("; term -> term SLASH factor code starts");
			generate(children(0), funcName);
			var push3Inst = "sw $3, -4($30)"
			var extendStackInst = "sub $30, $30, $4"

			MIPSOutput.append(push3Inst);
			MIPSOutput.append(extendStackInst);

			GenCodeForFactor.generate(children(2), funcName);
			var reduceStackInst = "add $30, $30, $4";
			var pop5Inst = "lw $5, -4($30)";

			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append(pop5Inst);
			val divThem = "div $5, $3";
			val mflo = "mflo $3";
			MIPSOutput.append(divThem);
			MIPSOutput.append(mflo) 
			MIPSOutput.append("; term -> term SLASH factor code ends")
		}
		else if (rule == "term term PCT factor") {
			MIPSOutput.append("; term -> term PCT factor code starts");
			generate(children(0), funcName);
			var push3Inst = "sw $3, -4($30)"
			var extendStackInst = "sub $30, $30, $4"

			MIPSOutput.append(push3Inst);
			MIPSOutput.append(extendStackInst);

			GenCodeForFactor.generate(children(2), funcName);
			var reduceStackInst = "add $30, $30, $4";
			var pop5Inst = "lw $5, -4($30)";

			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append(pop5Inst);
			val divThem = "div $5, $3";
			val mflo = "mfhi $3";
			MIPSOutput.append(divThem);
			MIPSOutput.append(mflo) 
			MIPSOutput.append("; term -> term PCT factor code ends")

		}
	}
}