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
	var MappingToRegisters = Map[String, String]();
	def init(mp: Map[String, String]) {
		MappingToRegisters =mp;
	}
	def processArglist(arglist: Node, funcName: String) {
		MIPSOutput.append("; processing ")
		val children = arglist.children;
		//println("arglist.rule " + arglist.rule + " children len " + children.length)
		if (arglist.rule == "arglist expr") {

			GenCodeForExpr.generate(children(0), funcName);
			Utils.push(3)
		}
		else {
			GenCodeForExpr.generate(children(0), funcName);
			Utils.push(3);
			processArglist(children(2), funcName);
		}
	}
	def generate(factor: Node, funcName: String) {
		val children = factor.children;
		val rule = factor.rule
		if (rule == "factor LPAREN expr RPAREN") {
			//println("for expr called with " +children(1).value)
			GenCodeForExpr.generate(children(1), funcName);
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
			generate(children(1), funcName);
			val takeAddress = "lw $3, 0($3)";
			MIPSOutput.append(takeAddress);
		}
		else if (rule == "factor AMP lvalue") {
			GenCodeForLvalue.generate(children(1), funcName)
		}
		else if (rule == "factor ID") {
			val id = children(0);
			val lex = id.lex;
		
			var offset = Utils.getValOfLexFromSymTable(lex, funcName).split(" ")(1);
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

			GenCodeForExpr.generate(children(3), funcName);
			MIPSOutput.append("; gen code for new expr")
			Utils.push(1);

			val set1 = "add $1, $0, $3";
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
			
			Utils.pop(1);
		}
		else if (rule == "factor ID LPAREN RPAREN") {
			Utils.push(29);

			Utils.push(31);

			MIPSOutput.append("lis $6");
			MIPSOutput.append(".word " + "F" + children(0).lex)
			MIPSOutput.append("sub $29, $30, $4");
			//MIPSOutput.append("sub $30, $30, $4")
			MIPSOutput.append("jalr $6")

			Utils.pop(31);

			Utils.pop(29);

		}
		else if (rule == "factor ID LPAREN arglist RPAREN") {
			Utils.push(29)
			
			MIPSOutput.append("; starting to process")
			

			Utils.push(31);

			MIPSOutput.append("add $6, $30, $0")
			
			processArglist(children(2), funcName);
			MIPSOutput.append("sub $29, $6, $4"); // HERE
			MIPSOutput.append("add $30, $6, $0"); //REMOVE THIS MAYBE?
			MIPSOutput.append("lis $6");
			MIPSOutput.append(".word " + "F" + children(0).lex)
			MIPSOutput.append("jalr $6")

			Utils.pop(31);
			Utils.pop(29);

		}
	}
}