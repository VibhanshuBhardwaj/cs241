import gen.Node;

import MIPSOutput._;
import GenCodeForFactor._;
import Utils._;

object GenCodeForTerm {
	val emptySet  = Set[String]();
	def generate(term: Node, funcName: String, regSet: Set[String]) : String = {
		val children = term.children;
		val rule = term.rule;
		if (isConstantTerm(term, funcName)) {
			var r = "";

			if (regSet.size == 0) r = "3"
			else r = regSet.head;

			val result = getNumTerm(term, funcName);
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
		else if (rule == "term factor") {
			//println("for factor called with " + children(0).value)
			val r = GenCodeForFactor.generate(children(0), funcName, regSet);
			MIPSOutput.append("; term -> factor r " + r);
			return r;
		}
		else if (rule == "term term STAR factor") {
			MIPSOutput.append("; term -> term STAR factor code starts");
			val s = generate(children(0), funcName, regSet);
			if (s == "3") { 
				Utils.push(3);

				GenCodeForFactor.generate(children(2), funcName, emptySet);
				Utils.pop(5);

				val multiplyThem = "mult $3, $5";
				val mflo = "mflo $3";
				MIPSOutput.append(multiplyThem);
				MIPSOutput.append(mflo);
				MIPSOutput.append("; term -> term STAR factor code ends");
				return "3";
			}
			else {
				var newSet = regSet - s;
				val t = GenCodeForFactor.generate(children(2), funcName, newSet);
				MIPSOutput.append("mult $" + s + ", $" + t);
				MIPSOutput.append("mflo $" + s);
				return s;
			}


		}
		else if (rule == "term term SLASH factor") {
			MIPSOutput.append("; term -> term SLASH factor code starts");
			val s = generate(children(0), funcName, regSet);
			if (s == "3") {
				Utils.push(3)

				GenCodeForFactor.generate(children(2), funcName, emptySet);
				Utils.pop(5)
				val divThem = "div $5, $3";
				val mflo = "mflo $3";
				MIPSOutput.append(divThem);
				MIPSOutput.append(mflo) 
				MIPSOutput.append("; term -> term SLASH factor code ends")
				return "3";
			}
			else {
				var newSet = regSet - s;
				val t = GenCodeForFactor.generate(children(2), funcName, newSet);
				MIPSOutput.append("div $" + s+ ", $" + t);
				MIPSOutput.append("mflo $" + s);
				return s;
			}
		}
		else if (rule == "term term PCT factor") {
			MIPSOutput.append("; term -> term PCT factor code starts");
			val s = generate(children(0), funcName, regSet);
			if (s == "3") {
				Utils.push(3)

				GenCodeForFactor.generate(children(2), funcName, emptySet);
				Utils.pop(5)
				val divThem = "div $5, $3";
				val mflo = "mfhi $3";
				MIPSOutput.append(divThem);
				MIPSOutput.append(mflo) 
				MIPSOutput.append("; term -> term PCT factor code ends");
				return "3";
			}
			else {
				val newSet = regSet - s;
				val t = GenCodeForFactor.generate(children(2), funcName, newSet);
				MIPSOutput.append("div $" + s + ", $" + t);
				MIPSOutput.append("mfhi $" + s);
				return s;


			}

		}
		return "3";
	}
}