import gen.Node;

import MIPSOutput._;
import GenCodeForFactor._;
import Utils._

object GenCodeForLvalue {
	def generate(lvalue: Node, funcName: String, regSet: Set[String]) : String = {
		//should only be called from inside generateCodeForFactor or recursively
		//ie. factor -> AMP lvalue;
		val children = lvalue.children;
		val rule = lvalue.rule;
		
		if (rule == "lvalue ID") {
			MIPSOutput.append("; CAREFULLY CHECK THIS. lvalue -> ID")
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
			return "3";
			
		}
		else if (rule == "lvalue STAR factor") {
			val r = GenCodeForFactor.generate(children(1), funcName, regSet);
			println(";r: " + r);
			return r;
		}
		else if (rule == "lvalue LPAREN lvalue RPAREN") {
			return generate(children(1), funcName, regSet);
		}
		else {
			println("; returning 3")
			return "3";
		}
	}
}