import gen.Node;

import MIPSOutput._;
import TypeChecker._;

import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.Map;

object GenCodeForTest {
	var FINALSYMTABLE =  ArrayBuffer[FunctionSymTable]();
	type FunctionSymTable = (String, Map[String, String], Int);
	def init(st: ArrayBuffer[FunctionSymTable]) {
		FINALSYMTABLE = st;
	}

	def generate(test: Node, funName: String) : Unit = {
		val children = test.children;
		val exp1 = children(0);
		val exp2 = children(2);
		GenCodeForExpr.generate(exp1, funName);
		var push3Inst = "sw $3, -4($30)"
		var extendStackInst = "sub $30, $30, $4"

		MIPSOutput.append(push3Inst);
		MIPSOutput.append(extendStackInst);
		GenCodeForExpr.generate(exp2, funName);
		var reduceStackInst = "add $30, $30, $4";
		var pop5Inst = "lw $5, -4($30)";
		MIPSOutput.append(reduceStackInst);
		MIPSOutput.append(pop5Inst);

		val exp1Type = TypeChecker.checkTypes(exp1, FINALSYMTABLE, funName);
		val exp2Type = TypeChecker.checkTypes(exp2, FINALSYMTABLE, funName);

		if (children(1).value == "LT") {
			MIPSOutput.append("; LT code")
			var sltInst = "slt $3, $5, $3";
			if (exp1Type =="int*") sltInst = "sltu $3, $5, $3"
			MIPSOutput.append(sltInst);
		}
		else if (children(1).value == "GT") {
			MIPSOutput.append("; GT code")
			var sltInst = "slt $3, $3, $5";
			if (exp1Type =="int*") sltInst = "sltu $3, $3, $5"
			MIPSOutput.append(sltInst);
		}
		else if (children(1).value == "GE") {
			MIPSOutput.append("; GE code. inverting LT code")
			var sltInst = "slt $3, $5, $3";
			if (exp1Type =="int*") sltInst = "sltu $3, $5, $3"
			val not3 = "sub $3, $11, $3";
			MIPSOutput.append(sltInst);
			MIPSOutput.append(not3);
		}
		else if (children(1).value == "LE") {
			MIPSOutput.append("; LE code. inverting GT code")
			var sltInst = "slt $3, $3, $5";
			if (exp1Type =="int*") sltInst = "sltu $3, $3, $5"
			val not3 = "sub $3, $11, $3";
			MIPSOutput.append(sltInst);
			MIPSOutput.append(not3);
		}
		else if (children(1).value == "NE") {
			MIPSOutput.append("; NE code");
			var sltInst1 = "slt $6, $3, $5";
			var sltInst2 = "slt $7, $5, $3";
			if (exp1Type =="int*") {
				sltInst1 = "sltu $6, $3, $5";
				sltInst2 = "sltu $7, $5, $3";
			}
			val add = "add $3, $6, $7";
			MIPSOutput.append(sltInst1);
			MIPSOutput.append(sltInst2);
			MIPSOutput.append(add);
		}
		else if (children(1).value == "EQ") {
			MIPSOutput.append("; EQ code. inverting NE");
			var sltInst1 = "slt $6, $3, $5";
			var sltInst2 = "slt $7, $5, $3";
			if (exp1Type =="int*") {
				sltInst1 = "sltu $6, $3, $5";
				sltInst2 = "sltu $7, $5, $3";
			}
			val add = "add $3, $6, $7";
			val not3 = "sub $3, $11, $3";
			MIPSOutput.append(sltInst1);
			MIPSOutput.append(sltInst2);
			MIPSOutput.append(add);
			MIPSOutput.append(not3);

		}
	}

}