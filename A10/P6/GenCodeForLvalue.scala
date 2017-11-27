import gen.Node;

import MIPSOutput._;
import GenCodeForFactor._;
import Utils._

object GenCodeForLvalue {
	def generate(lvalue: Node, funcName: String) : Unit = {
		//should only be called from inside generateCodeForFactor or recursively
		//ie. factor -> AMP lvalue;
		val children = lvalue.children;
		val rule = lvalue.rule;
		if (rule == "lvalue ID") {
			val lex = children(0).lex;
			val offset = Utils.getValOfLexFromSymTable(lex, funcName).split(" ")(1);
			val lis3 = "lis $3"
			var dotWordOffset = ".word "

			if (offset == "0") dotWordOffset+= offset;
			else {
				dotWordOffset+= "-"
				dotWordOffset+=offset
			}
			

			MIPSOutput.append(lis3);
			MIPSOutput.append(dotWordOffset);
			val storeAddress = "add $3, $3, $29";
			MIPSOutput.append(storeAddress);
			
		}
		else if (rule == "lvalue STAR factor") {
			GenCodeForFactor.generate(children(1), funcName);
		}
		else if (rule == "lvalue LPAREN lvalue RPAREN") {
			generate(children(1), funcName);
		}
	}
}