import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map;

import gen.Node;

import MIPSOutput._;
import GenCodeForLvalue._;
import GenCodeForExpr._;
import Utils._;

object GenCodeForFactor {
	val availableRegisters = Array("9", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27");
	var MappingToRegisters = Map[String, String]();
	def init(mp: Map[String, String]) {
		MappingToRegisters =mp;
	}
	def processArglist(arglist: Node, funcName: String, regSet: Set[String]) {
		MIPSOutput.append("; processing ")
		val children = arglist.children;
		//println("arglist.rule " + arglist.rule + " children len " + children.length)
		if (arglist.rule == "arglist expr") {

			GenCodeForExpr.generate(children(0), funcName, regSet);
			Utils.push(3)
		}
		else {
			GenCodeForExpr.generate(children(0), funcName, regSet);
			Utils.push(3);
			processArglist(children(2), funcName, regSet);
		}
	}
	def generate(factor: Node, funcName: String, regSet: Set[String]) : String = {
		val children = factor.children;
		val rule = factor.rule
		if (rule == "factor LPAREN expr RPAREN") {
			//println("for expr called with " +children(1).value)
			return GenCodeForExpr.generate(children(1), funcName, regSet);
		}
		else if (rule == "factor NUM") {
			var r = "";

			if (regSet.size == 0) r = "3"
			else r = regSet.head;

			val num = children(0);
			val lex = num.lex;
			val loadWordTo3 = "lis $" + r;
			var constToBeLoaded = ".word " + lex;
			MIPSOutput.append(loadWordTo3);
			MIPSOutput.append(constToBeLoaded);
			return r;
		}
		else if (rule =="factor STAR factor") {
			MIPSOutput.append("; pointers! factor -> STAR factor")
			val r = generate(children(1), funcName, regSet);
			val takeAddress = "lw $" + r +", 0($" + r + ")";
			MIPSOutput.append(takeAddress);
			return r;
		}
		else if (rule == "factor AMP lvalue") {
			return GenCodeForLvalue.generate(children(1), funcName, regSet);
			//return "3"
		}
		else if (rule == "factor ID") {
			val id = children(0);
			val lex = id.lex;
			var r = "";
			val fullName = funcName + " " + lex;

			if (MappingToRegisters contains fullName) {
				r = MappingToRegisters(fullName);
				var tt = ""
				if (regSet.size == 0) tt = "3"
				else tt = regSet.head;
				MIPSOutput.append("add $" + tt + ", $" + r + ", $0");
				return tt;
			}
			else {
				if (regSet.size == 0) r = "3"
				else r = regSet.head;
				var offset = Utils.getValOfLexFromSymTable(lex, funcName).split(" ")(1);
				var inst ="";
				if (offset == "0") inst+= "lw $" + r + ", "
				else inst+= "lw $" + r + ", -"
				inst+= offset.toString;
				inst+="($29)"
				MIPSOutput.append(inst);
				return r;
			}
		}
		else if (rule == "factor NULL") { //FLAG
			val set3ToNull = "add $3, $0, $11";
			MIPSOutput.append(set3ToNull)
			return "3"
		}
		else if (rule == "factor NEW INT LBRACK expr RBRACK") {

			var s = GenCodeForExpr.generate(children(3), funcName, regSet);
			MIPSOutput.append("; gen code for new expr")
			Utils.push(1);

			val set1 = "add $1, $0, $" + s;
			MIPSOutput.append(set1);
			val lis10 = "lis $10";
			val newWord = ".word new"
			val call = "jalr $10"
			MIPSOutput.append(lis10);
			MIPSOutput.append(newWord);
			MIPSOutput.append(call);

			var reduceStackInst = "add $30, $30, $4";

			val r = scala.util.Random;
			val randInt = r.nextInt(1000);
			MIPSOutput.append("bne $3, $0, newSuccess" + randInt.toString);
			MIPSOutput.append("add $3, $11, $0");
			MIPSOutput.append("newSuccess" + randInt.toString + ":")
			MIPSOutput.append("add $" + s + ", $0, $3")
			
			Utils.pop(1);
			return s;
		}
		else if (rule == "factor ID LPAREN RPAREN") {
			Utils.push(29);

			Utils.push(31);

			var regUsed = (availableRegisters.toSet -- regSet).toArray;
			MIPSOutput.append("; saving registers")
			for (c<- regUsed) {
				println("; c is " + c)
				Utils.push(c.toString.toInt)
			}
			MIPSOutput.append("lis $10");
			MIPSOutput.append(".word " + "F" + children(0).lex)
			MIPSOutput.append("sub $29, $30, $4");
			//MIPSOutput.append("sub $30, $30, $4")
			MIPSOutput.append("jalr $10")

			regUsed = regUsed.reverse;
			MIPSOutput.append("; restoring registers")
			for (c<- regUsed) {
				Utils.pop(c.toString.toInt);
			}

			Utils.pop(31);

			Utils.pop(29);
			return "8"


		}
		else if (rule == "factor ID LPAREN arglist RPAREN") {
			Utils.push(29)
			
			MIPSOutput.append("; starting to process")
			

			Utils.push(31);

			MIPSOutput.append("add $21, $30, $0")
			
			processArglist(children(2), funcName, regSet);
			MIPSOutput.append("sub $29, $21, $4"); // HERE
			MIPSOutput.append("add $30, $21, $0"); //REMOVE THIS MAYBE?
			MIPSOutput.append("lis $10");
			MIPSOutput.append(".word " + "F" + children(0).lex)
			MIPSOutput.append("jalr $10")

			Utils.pop(31);
			Utils.pop(29);

		}
		return "3";
	}
}