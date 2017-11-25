import gen.Node;

import MIPSOutput._;

object GenCodeForTest {
	def generate(test: Node) : Unit = {
		val children = test.children;
		val exp1 = children(0);
		val exp2 = children(2);
		GenCodeForExpr.generate(exp1);
		var push3Inst = "sw $3, -4($30)"
		var extendStackInst = "sub $30, $30, $4"

		MIPSOutput.append(push3Inst);
		MIPSOutput.append(extendStackInst);
		GenCodeForExpr.generate(exp2);
		var reduceStackInst = "add $30, $30, $4";
		var pop5Inst = "lw $5, -4($30)";
		MIPSOutput.append(reduceStackInst);
		MIPSOutput.append(pop5Inst)
		
		if (children(1).value == "LT") {
			MIPSOutput.append("; LT code")
			val sltInst = "slt $3, $5, $3";
			MIPSOutput.append(sltInst);
		}
		else if (children(1).value == "GT") {
			MIPSOutput.append("; GT code")
			val sltInst = "slt $3, $3, $5";
			MIPSOutput.append(sltInst);
		}
		else if (children(1).value == "GE") {
			MIPSOutput.append("; GE code. inverting LT code")
			val sltInst = "slt $3, $5, $3";
			val not3 = "sub $3, $11, $3";
			MIPSOutput.append(sltInst);
			MIPSOutput.append(not3);
		}
		else if (children(1).value == "LE") {
			MIPSOutput.append("; LE code. inverting GT code")
			val sltInst = "slt $3, $3, $5";
			val not3 = "sub $3, $11, $3";
			MIPSOutput.append(sltInst);
			MIPSOutput.append(not3);
		}
		else if (children(1).value == "NE") {
			MIPSOutput.append("; NE code");
			val sltInst1 = "slt $6, $3, $5";
			val sltInst2 = "slt $7, $5, $3";
			val add = "add $3, $6, $7";
			MIPSOutput.append(sltInst1);
			MIPSOutput.append(sltInst2);
			MIPSOutput.append(add);
		}
		else if (children(1).value == "EQ") {
			MIPSOutput.append("; EQ code. inverting NE");
			val sltInst1 = "slt $6, $3, $5";
			val sltInst2 = "slt $7, $5, $3";
			val add = "add $3, $6, $7";
			val not3 = "sub $3, $11, $3";
			MIPSOutput.append(sltInst1);
			MIPSOutput.append(sltInst2);
			MIPSOutput.append(add);
			MIPSOutput.append(not3);

		}
	}

}