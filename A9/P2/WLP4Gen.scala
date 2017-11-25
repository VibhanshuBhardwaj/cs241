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
	}
	def generateCodeForTerm(term: Node) : Unit = {
		val children = term.children;
		if (term.rule == "term factor") {
			//println("for factor called with " + children(0).value)
			generateCodeForFactor(children(0));
		}
	}
	def generateCodeForFactor(factor: Node) : Unit = {
		val children = factor.children;
		if (factor.rule == "factor LPAREN expr RPAREN") {
			//println("for expr called with " +children(1).value)
			generateCodeForExpr(children(1));
		}
		else if (factor.rule == "factor ID") {
			val id = children(0);
			val lex = id.lex;
			for (f<- FINALSYMTABLE) {
		
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