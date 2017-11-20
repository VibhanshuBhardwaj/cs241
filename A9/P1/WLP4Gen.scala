import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map; //[String, Int]()

import gen.Node;
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
		val expr = children(11);
		val id = expr.children(0).children(0).children(0);
		val lex = id.lex;
		
	}
	def main(args: Array[String]) : Unit = {
		var ParseTree = new Node("ROOT", "");
		ParseTree = ParseTreeBuilder.construct(ParseTree).children(0);

		var symTable = ArrayBuffer[FunctionSymTable]();
		symTable = SymbolTableBuilder.buildSymbolTable(ParseTree, symTable, "");
		//generateCode(ParseTree);
		SymbolTableBuilder.debugPrintSymTable(symTable);
		FINALSYMTABLE = symTable;

	}

}