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

	def generateCode(proceduresTree: Node) : Unit = {
		val children = proceduresTree.children;
		val mainTree = children(0);
		MIPSOutput.addProlog(2); //replace with actual size...
		//println("main tree? " + mainTree.value + " children len "  + mainTree.children.length)
		val expr = mainTree.children(11);
		val id = expr.children(0).children(0).children(0);
		val lex = id.lex;
		for (f<- FINALSYMTABLE) {
		//	println("f._1 " + f._1)
			if (f._1 == "wain") {
				//println("does not happen eh?")
		//		println( "f._2(lex) " + f._2(lex))
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