import gen.Node;

import MIPSOutput._;
import GenCodeForLvalue._;
import Utils._;

object GenCodeForFactor {
	def generate(factor: Node) {
		val children = factor.children;
		val rule = factor.rule
		if (rule == "factor LPAREN expr RPAREN") {
			//println("for expr called with " +children(1).value)
			GenCodeForExpr.generate(children(1));
		}
		else if (rule == "factor NUM") {
			val num = children(0);
			val lex = num.lex;
			val loadWordTo3 = "lis $3";
			var constToBeLoaded = ".word " + lex;
			MIPSOutput.append(loadWordTo3);
			MIPSOutput.append(constToBeLoaded);
		}
		else if (rule =="factor STAR factor") {
			MIPSOutput.append("; pointers! factor -> STAR factor")
			generate(children(1));
			val takeAddress = "lw $3, 0($3)";
			MIPSOutput.append(takeAddress);
		}
		else if (rule == "factor AMP lvalue") {
			GenCodeForLvalue.generate(children(1))
		}
		else if (rule == "factor ID") {
			val id = children(0);
			val lex = id.lex;
		
			var offset = Utils.getValOfLexFromSymTable(lex).split(" ")(1);
			var inst ="";
			if (offset == "0") inst+= "lw $3, "
			else inst+= "lw $3, -"
			inst+= offset.toString;
			inst+="($29)"
			MIPSOutput.append(inst);
		}
		else if (rule == "factor NULL") {
			val set3ToNull = "add $3, $0, $11";
			MIPSOutput.append(set3ToNull)
		}
		else if (rule == "factor NEW INT LBRACK expr RBRACK") {

			GenCodeForExpr.generate(children(3));
			MIPSOutput.append("; gen code for new expr")
			val store1 = "sw $1, -4($30)";
			val extendStackInst = "sub $30, $30, $4";
			val set1 = "add $1, $0, $3";
			MIPSOutput.append(store1);
			MIPSOutput.append(extendStackInst);
			MIPSOutput.append(set1);
			val lis10 = "lis $10";
			val newWord = ".word new"
			val call = "jalr $10"
			MIPSOutput.append(lis10);
			MIPSOutput.append(newWord);
			MIPSOutput.append(call);
			var reduceStackInst = "add $30, $30, $4";
			var pop5Inst = "lw $1, -4($30)";
			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append(pop5Inst);
		}
	}
}