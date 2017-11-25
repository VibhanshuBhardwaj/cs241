import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map; //[String, Int]()

import gen.Node;

object WLP4Gen {
	var input = ArrayBuffer[String]();
	var in = Source.fromInputStream(System.in).getLines;
	var signatureMap = Map[String, String]();
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

	def addSymTable(dcl: Node, symTable:  ArrayBuffer[FunctionSymTable], scope: String) : ArrayBuffer[FunctionSymTable] = {
		var newSymTable = ArrayBuffer[FunctionSymTable]();
		newSymTable = symTable;
		var mapping = Map[String, String]();
		val dcl1Tree = dcl;
		val dcl1TypeTree = dcl1Tree.children(0);
		val dcl1ID = dcl1Tree.children(1).lex;
		for (fTable<- symTable) {
			if (scope == fTable._1) {
				if (fTable._2.contains(dcl1ID)) {
					Console.err.println("ERROR")
					Console.err.println("REDIFINING A Variable IS NOT ALLOWED. You have multiple definitions of " + dcl1ID);
					System.exit(1);
				}
			}
		}
		var dcl1Type = "";
		for (c<- dcl1TypeTree.children) {
			dcl1Type = dcl1Type + c.lex;
		}
		//println(dcl1ID + " " + dcl1Type);
		// = 
		//mapping = mapping + (dcl1ID -> dcl1Type);
		var found = false;
		for (i<-0 to newSymTable.length - 1) {
			if (newSymTable(i)._1== scope) {
				var oldMapping = newSymTable(i)._2;
				newSymTable(i) =  (scope, oldMapping + (dcl1ID -> dcl1Type) );
				found = true;
			}
		}
		if (!found) {
			var fTable: FunctionSymTable = (scope, mapping + (dcl1ID -> dcl1Type));
			newSymTable+=fTable;
		}
	//printSymbolTable(newSymTable);
		return newSymTable;
	}
	def getTypeDCL(dcl: Node) : String = {
		val dcl1Tree = dcl;
		val dcl1TypeTree = dcl1Tree.children(0);
		val dcl1ID = dcl1Tree.children(1).lex;
		var dcl1Type = "";
		for (c<- dcl1TypeTree.children) {
			dcl1Type = dcl1Type + c.lex;
		}
		return dcl1Type;

	}
	def buildSymbolTable(tree: Node, symTable: ArrayBuffer[FunctionSymTable], inScopeOf: String) : ArrayBuffer[FunctionSymTable] = {
		
		if (isTerminal(tree.value)) return symTable;
		else if (tree.value == "procedures") {
			//println("one dcl")
			var newSymTable = ArrayBuffer[FunctionSymTable]();
			newSymTable = symTable;
			var newScope = inScopeOf;
			if (tree.children.length == 1 && tree.children(0).value == "main") {
				newScope = "wain";
				var MainChildren = tree.children(0).children;
				for (c <- MainChildren) {
					if (c.value == "dcl") {
						if (signatureMap.contains("wain")) {
							signatureMap("wain") += " " + getTypeDCL(c);
						}
						else {
							signatureMap = signatureMap + ("wain" -> getTypeDCL(c))
						}
						newSymTable = addSymTable(c, newSymTable, "wain");
					}
					else newSymTable = buildSymbolTable(c, newSymTable, "wain");
				}
				return newSymTable
			}
			else { //handle procedures
				var firstProcedure = tree.children(0);
				//println("handling general procedure " + firstProcedure.value)
				var newSymTable = ArrayBuffer[FunctionSymTable]();
				newSymTable = symTable;
				var newScope = firstProcedure.children(1).lex;
				//ADD CHECK HERE
				if (signatureMap.contains(newScope)) {
					Console.err.println("ERROR")
					Console.err.println("REDIFINING a function is not allowed")
					System.exit(1);
				}
				else {
					signatureMap = signatureMap + (newScope -> "")
				}
				//println(" new scope " + newScope)
				for (c <- firstProcedure.children) {
					//println("first procedure child " + c.value)
					if (c.value == "dcl") newSymTable = addSymTable(c, newSymTable, newScope);
					else {
						if (c.value == "paramlist") {
							println("happens")
							var paramDCL = c.children(0);
							var typeTree = paramDCL.children(0);
							var DCLtype = ""
							for (c<- typeTree.children) {
								DCLtype = DCLtype + c.lex;
							}
							if (signatureMap.contains(newScope) && signatureMap(newScope)=="") {
								//println()
								signatureMap(newScope) += DCLtype;
							}
							else {
								signatureMap(newScope) += " " + DCLtype;
							}

						}
						newSymTable = buildSymbolTable(c, newSymTable, newScope);
					}
				}
				newSymTable = buildSymbolTable(tree.children(1), newSymTable, newScope);
				return newSymTable;
			}		
		}
			
		else {
			var newSymTable = symTable
			for (c <- tree.children) {
				if (c.value == "dcl") newSymTable = addSymTable(c, newSymTable, inScopeOf);
				else {
					if (c.value == "paramlist") {
						//println("happens")
						var paramDCL = c.children(0);
						var typeTree = paramDCL.children(0);
						var DCLtype = ""
						for (c<- typeTree.children) {
							DCLtype = DCLtype + c.lex;
						}
						if (signatureMap.contains(inScopeOf) && signatureMap(inScopeOf)=="") {
							//println()
							signatureMap(inScopeOf) += DCLtype;
						}
						else {
							signatureMap(inScopeOf) += " " + DCLtype;
						}

					}
					newSymTable = buildSymbolTable(c, symTable, inScopeOf);
				}
			}
			return newSymTable;
		}
	}
	def printSymbolTable(symTable: ArrayBuffer[FunctionSymTable]) {
		for (fTable <- symTable) {
			if (fTable._1 == "wain") {
				Console.err.println("wain " + signatureMap("wain")); //function name
				for ((k, v) <- fTable._2) {
					Console.err.println(k + " " + v)
				}
			}
		}
	}
	def checkIfUndefinedVarsInStatements(tree: Node, symTable: ArrayBuffer[FunctionSymTable], inScopeOf: String) : Boolean =  {
		if (tree.children.length == 0) return false;
		var statement = tree.children(1);
		for (c <- statement.children) {
			if (c.value == "expr") {
				//println("happening expr insinde statements")
				if (checkIfUndefinedVarsInExpr(c, symTable, inScopeOf)) return true;
			}
			else if (c.value == "statements") { 
				if (checkIfUndefinedVarsInStatements(c, symTable, inScopeOf)) return true;
			}
		}
		if (checkIfUndefinedVarsInStatements(tree.children(0), symTable, inScopeOf)) return true;
		else return false;

	}
	def checkIfUndefinedVarsInExpr(tree: Node, symTable: ArrayBuffer[FunctionSymTable], inScopeOf: String) : Boolean =  {
		if (tree.children.length == 0) return false;
		for (c<-tree.children) {
			if (c.value =="expr") { 
				//println("happening expr insinde expr")
				if (checkIfUndefinedVarsInExpr(c, symTable, inScopeOf)) return true;
			}
			else if (c.value == "term") { 
				if (checkIfUndefinedVarsInTerm(c, symTable, inScopeOf)) return true;
			}
		}
		return false;

	}
	def checkIfUndefinedVarsInTerm(tree: Node, symTable: ArrayBuffer[FunctionSymTable], inScopeOf: String) : Boolean =  {
		if (tree.children.length == 0) return false;
		for (c<- tree.children) {

			if (c.value=="factor") {
				//println("happening factor insinde term")
				if (checkIfUndefinedVarsInFactor(c, symTable, inScopeOf)) return true;
			}
			
			else if (c.value == "term") { 
				if (checkIfUndefinedVarsInTerm( c, symTable, inScopeOf)) return true;
			}
		}
		return false;
	}
	def checkIfUndefinedVarsInFactor(tree: Node, symTable: ArrayBuffer[FunctionSymTable], inScopeOf: String) : Boolean =  {
		if (tree.children.length == 0) return false;
		for (c<- tree.children) {
			if (c.value == "ID") {
				//println("c value is ID. lex is " + c.lex )
				if (tree.rule.contains("LPAREN")) { //ie it's a function call
					for (f<- symTable) {
						if (f._1 == c.lex) return false;
					}
					return false;
				}
				else {
					for (f<- symTable) {
						if (f._1 == inScopeOf) {
							if (!f._2.contains(c.lex)) return true;
						}
					}
				}
			}
			else if (c.value == "factor") {
				if (checkIfUndefinedVarsInFactor(c, symTable, inScopeOf)) return true;
			}
			else if (c.value =="expr") { 
				if (checkIfUndefinedVarsInExpr(c, symTable, inScopeOf)) return true;
			}
			else if (c.value == "arglist") {
				if (checkIfUndefinedVarsInArglist(c, symTable, inScopeOf)) return true;
			}
		}
		return false;
	}
	def checkIfUndefinedVarsInArglist(tree: Node, symTable: ArrayBuffer[FunctionSymTable], inScopeOf: String) : Boolean =  {
		if (tree.children.length == 0) return false;
		for (c<- tree.children) {
			if (c.value == "arglist") {
				if (checkIfUndefinedVarsInArglist(c, symTable, inScopeOf)) return true;
			}
			else if (c.value == "expr") {
				if (checkIfUndefinedVarsInExpr(c, symTable, inScopeOf)) return true;
			}
		}
		return false;
	}
	def checkIfUndefinedVarsInProcedures(tree: Node, symTable: ArrayBuffer[FunctionSymTable]) : Boolean = {
		if (tree.children.length == 0) return false;
		else if (tree.children.length == 1 && tree.children(0).value == "main") {
			var mainTree = tree.children(0);
			for (c <- mainTree.children) {
				if (c.value=="statements") {
					if (checkIfUndefinedVarsInStatements(c, symTable, "wain")) return true;
				}
				else if (c.value=="expr") {
					//println("shud be happening")
					if (checkIfUndefinedVarsInExpr(c, symTable, "wain")) return true;
				}
			}
			return false;
		}
		else {
			var firstProcedure = tree.children(0);
			var firstProcedureName = firstProcedure.children(1).lex;
			for (c <- firstProcedure.children) {
				if (c.value=="statements") {
					if (checkIfUndefinedVarsInStatements(c, symTable, firstProcedureName)) return true;
				}
				else if (c.value=="expr") {
					//println("shud be happening")
					if (checkIfUndefinedVarsInExpr(c, symTable, firstProcedureName)) return true;
				}
			}
			return checkIfUndefinedVarsInProcedures(tree.children(1), symTable);
		}
	}
	def checkIfUndefinedVars(tree: Node, symTable: ArrayBuffer[FunctionSymTable]) : Boolean = {
		if (tree.children.length == 0) return false;
		for (c<- tree.children) {
			if (c.value == "procedures") {
				if (checkIfUndefinedVarsInProcedures(c, symTable)) return true;
			}
			else if(c.value=="statements") {
				if (checkIfUndefinedVarsInStatements(c, symTable, "main")) return true;
			}
			else if (c.value=="expr") {
				//println("shud be happening")
				if (checkIfUndefinedVarsInExpr(c, symTable, "main")) return true;
			}
		}
		return false;
	}
	def printSignatures() {
		for ((k, v) <- signatureMap) {
			if(k!="wain") {
				Console.err.println(k + " " + v);
				Console.err.println("")
			}
		}
	}
	def main(args: Array[String]) : Unit = {
		var ParseTree = new Node("ROOT", "");
		ParseTree = buildParseTree(ParseTree, in.next).children(0);
		//printTree(ParseTree)
		var symTable = ArrayBuffer[FunctionSymTable]();
		symTable = buildSymbolTable(ParseTree, symTable, "");
		var undefinedVars = checkIfUndefinedVars(ParseTree, symTable);
		if (undefinedVars) {
			Console.err.println("ERROR");
			Console.err.println("undefined variable");
			System.exit(1);
		}
		printSignatures();
		printSymbolTable(symTable);


	}

}