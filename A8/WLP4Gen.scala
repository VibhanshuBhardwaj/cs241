import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map; //[String, Int]()

import gen.Node;

object WLP4Gen {
	var input = ArrayBuffer[String]();
	var in = Source.fromInputStream(System.in).getLines;

	type FunctionSymTable = (String, Map[String, String]);
	def readInput() : Unit = {
		var in = Source.fromInputStream(System.in).getLines;
		
		while (in.hasNext) {
			input += in.next;
		}
		
	}
	def isTerminal(s: String): Boolean =  {
		return s == s.toUpperCase
	}

	def printTree(tree: Node) : Unit = {
		println("node val " + tree.value);
		println("node derivation " + tree.rule)
		tree.children.map(x=>print(x.value + " <3 "))
		println("")
		for (c<- tree.children) {
			printTree(c)
		}
	}
	def buildParseTree(tree: Node, derivation: String) : Node = { //ADD A BASE CASE for derivation being null
		//ADD A BASE CASE
		if (derivation == "") return tree;
		var derivationArr = derivation.split(" ");
		val LHS = derivationArr(0);
		var RHS = derivationArr.drop(1);
		//if (LHS == "start") println("RHS len " + RHS.length )
		if (isTerminal(LHS)) {

			tree.children += new Node(LHS, derivation);


			tree.children(tree.children.length - 1).lex = RHS(0);
		}
		else {
			tree.children += new Node(LHS, derivation)
			for (r <- RHS) {
				//if (LHS == "start") println("rhs r " + r);
				var next = in.next;

				buildParseTree(tree.children(tree.children.length - 1), next)

			}
		}
		return tree;
	}
	def buildSymbolTable(tree: Node, symTable: ArrayBuffer[FunctionSymTable]) : ArrayBuffer[FunctionSymTable] = {
		
		if (isTerminal(tree.value)) return symTable;
		else if (tree.value == "main") {
			var newSymTable = ArrayBuffer[FunctionSymTable]();
			newSymTable = symTable;
			var mapping = Map[String, String]();
			//println("mains children are ")
			//tree.children.map(x=>print(x.value + " "))
			//val dcl1Tree = tree.children(3);
			//val dcl2Tree = tree.children(5);
			//println("got trees " + dcl1Tree.value + " " + dcl2Tree.value)
			for (mainChild<- tree.children) {
				if (mainChild.value == "dcl") { 
					val dcl1Tree = mainChild;
					val dcl1TypeTree = dcl1Tree.children(0);
					val dcl1ID = dcl1Tree.children(1).lex;
					var dcl1Type = "";
					for (c<- dcl1TypeTree.children) {
						dcl1Type = dcl1Type + c.lex;
					}
					//println(dcl1ID + " " + dcl1Type);
					// = 
					mapping = mapping + (dcl1ID -> dcl1Type);
				}
			}
			var waintable: FunctionSymTable = ("wain", mapping);
			newSymTable+=waintable;
			return newSymTable;

		}
		else {
			var newSymTable = symTable
			for (c <- tree.children) {
				newSymTable = buildSymbolTable(c, symTable);
			}
			return newSymTable;
		}
	}
	def printSymbolTable(symTable: ArrayBuffer[FunctionSymTable]) {
		for (fTable <- symTable) {
			println(fTable._1); //function name
			for ((k, v) <- fTable._2) {
				println(k + " " + v)
			}
		}
	}
	def main(args: Array[String]) : Unit = {
		var ParseTree = new Node("ROOT", "");
		ParseTree = buildParseTree(ParseTree, in.next).children(0);
		//printTree(ParseTree)
		var symTable = ArrayBuffer[FunctionSymTable]();
		symTable = buildSymbolTable(ParseTree, symTable);
		printSymbolTable(symTable);


	}

}