import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map; //[String, Int]()

import gen.Node;

import MIPSOutput._;
import TypeChecker._;
import SymbolTableBuilder._;
import ParseTreeBuilder._;

object WLP4Gen {
	var input = ArrayBuffer[String]();
	
	var signatureMap = Map[String, String]();
	var FINALSYMTABLE = ArrayBuffer[FunctionSymTable]();

	type FunctionSymTable = (String, Map[String, String], Int);
	def generateCodeForExpr(expr: Node) : Unit = {
		val children = expr.children;

		if (expr.rule == "expr term") {
			//println("for term called with " + children(0).value)
			generateCodeForTerm(children(0));
		}

		else if (expr.rule == "expr expr PLUS term") {
			MIPSOutput.append("; expr -> expr PLUS term");
			generateCodeForExpr(children(0));

			var push3Inst = "sw $3, -4($30)"
			var extendStackInst = "sub $30, $30, $4"

			MIPSOutput.append(push3Inst);
			MIPSOutput.append(extendStackInst);

			generateCodeForTerm(children(2));

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

			generateCodeForTerm(children(2));

			var reduceStackInst = "add $30, $30, $4";
			var pop5Inst = "lw $5, -4($30)";

			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append(pop5Inst);
			var finalAddInst = "sub $3, $5, $3";
			MIPSOutput.append(finalAddInst);
			
		}
	}
	def generateCodeForTerm(term: Node) : Unit = {
		val children = term.children;
		val rule = term.rule;
		if (rule == "term factor") {
			//println("for factor called with " + children(0).value)
			generateCodeForFactor(children(0));
		}
		else if (rule == "term term STAR factor") {
			MIPSOutput.append("; term -> term STAR factor code starts");
			generateCodeForTerm(children(0));
			var push3Inst = "sw $3, -4($30)"
			var extendStackInst = "sub $30, $30, $4"

			MIPSOutput.append(push3Inst);
			MIPSOutput.append(extendStackInst);

			generateCodeForFactor(children(2));
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
			generateCodeForTerm(children(0));
			var push3Inst = "sw $3, -4($30)"
			var extendStackInst = "sub $30, $30, $4"

			MIPSOutput.append(push3Inst);
			MIPSOutput.append(extendStackInst);

			generateCodeForFactor(children(2));
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
			generateCodeForTerm(children(0));
			var push3Inst = "sw $3, -4($30)"
			var extendStackInst = "sub $30, $30, $4"

			MIPSOutput.append(push3Inst);
			MIPSOutput.append(extendStackInst);

			generateCodeForFactor(children(2));
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
	def generateCodeForFactor(factor: Node) : Unit = {
		val children = factor.children;
		val rule = factor.rule
		if (rule == "factor LPAREN expr RPAREN") {
			//println("for expr called with " +children(1).value)
			generateCodeForExpr(children(1));
		}
		else if (rule == "factor NUM") {
			val num = children(0);
			val lex = num.lex;
			val loadWordTo3 = "lis $3";
			var constToBeLoaded = ".word " + lex;
			MIPSOutput.append(loadWordTo3);
			MIPSOutput.append(constToBeLoaded);
		}

		else if (rule == "factor ID") {
			val id = children(0);
			val lex = id.lex;
			for (f <- FINALSYMTABLE) {
		
				if (f._1 == "wain") {
					var offset = f._2(lex).split(" ")(1);
			//		println("offset from lex "  + offset);
					var inst ="";
					if (offset == "0") inst+= "lw $3, "
					else inst+= "lw $3, -"
					inst+= offset.toString;
					inst+="($29)"
					MIPSOutput.append(inst);
				}
			}
		}
	}
	def generateCode(proceduresTree: Node) : Unit = {
		val children = proceduresTree.children;
		val mainTree = children(0);
		MIPSOutput.addProlog(2); //replace with actual size...
		//println("main tree? " + mainTree.value + " children len "  + mainTree.children.length)
		val expr = mainTree.children(11);
		generateCodeForExpr(expr);
		//val id = expr.children(0).children(0);
		//val lex = id.lex;
		
		
		MIPSOutput.addEpilog();
		MIPSOutput.printOutput();
		
	}
	def main(args: Array[String]) : Unit = {
		var ParseTree = new Node("ROOT", "");
		ParseTree = ParseTreeBuilder.construct(ParseTree).children(0);

		var symTable = ArrayBuffer[FunctionSymTable]();
		symTable = SymbolTableBuilder.buildSymbolTable(ParseTree, symTable, "");
		val procedures = ParseTree.children(1);
		FINALSYMTABLE = symTable;
		generateCode(procedures);
		//SymbolTableBuilder.debugPrintSymTable(symTable);
		

	}

}