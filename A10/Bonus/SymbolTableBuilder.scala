import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map;

import gen.Node;

object SymbolTableBuilder {

	type FunctionSymTable = (String, Map[String, String], Int);
	var signatureMap = Map[String, String]();

	def isTerminal(s: String): Boolean =  {
		return s == s.toUpperCase
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
				val offset = newSymTable(i)._3.toString;
				dcl1Type = dcl1Type + " " + offset;
				newSymTable(i) =  (scope, oldMapping + (dcl1ID -> dcl1Type), newSymTable(i)._3 + 4 );
				found = true;
			}
		}
		if (!found) {
			dcl1Type = dcl1Type + " 0"
			var fTable: FunctionSymTable = (scope, mapping + (dcl1ID -> dcl1Type), 4);
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
	def addSymTableFromParams(tree: Node, symTable: ArrayBuffer[FunctionSymTable], inScopeOf: String) : ArrayBuffer[FunctionSymTable] = {
		//println("adding from param " + tree.value)
		val children = tree.children;
		val dcl = children(0);
		var newSymTable = symTable;
		if (children.length > 1) {
			newSymTable = addSymTable(dcl, newSymTable, inScopeOf);
			return addSymTableFromParams(children(2), newSymTable, inScopeOf);
		}
		else {
			newSymTable = addSymTable(dcl, newSymTable, inScopeOf);
			return newSymTable;
		}
	}
	def addSymTableFromDCLs(tree: Node, symTable: ArrayBuffer[FunctionSymTable], inScopeOf: String) : ArrayBuffer[FunctionSymTable] = {
		var children = tree.children;
		if (children.length > 0) {
			var newSymTable = symTable;
			newSymTable = addSymTableFromDCLs(children(0), newSymTable, inScopeOf);
			newSymTable = addSymTable(children(1), newSymTable, inScopeOf);
			
			return newSymTable;
		}
		else return symTable;
	}
	def buildSymbolTable(tree: Node, symTable: ArrayBuffer[FunctionSymTable], inScopeOf: String) : ArrayBuffer[FunctionSymTable] = {
		
		if (isTerminal(tree.value)) return symTable;
		else if (tree.value == "dcls") {
			return addSymTableFromDCLs(tree, symTable, inScopeOf);
		}
		else if (tree.value == "procedures") {
			var newSymTable = symTable;
			var newScope = inScopeOf;

			if (tree.children.length == 1 && tree.children(0).value == "main") {
				newScope = "wain";
				var MainChildren = tree.children(0).children;
				for (c <- MainChildren) {
					if (c.value == "dcl") {
						newSymTable = addSymTable(c, newSymTable, "wain")
					}
					else newSymTable = buildSymbolTable(c, newSymTable, "wain");
				}
				return newSymTable
			}
			else { //handle procedures
				var firstProcedure = tree.children(0);
				//println("handling procedures")
				var newSymTable = ArrayBuffer[FunctionSymTable]();
				newSymTable = symTable;
				var newScope = firstProcedure.children(1).lex;
				//println("handling general procedure " + newScope)
				//ADD CHECK HERE
				if (signatureMap.contains(newScope)) {
					Console.err.println("ERROR")
					Console.err.println("REDIFINING a function is not allowed")
					System.exit(1);
				}
				else {
					signatureMap = signatureMap + (newScope -> "")
					val emptyMap = Map[String, String]();
					var fTable: FunctionSymTable = (newScope, emptyMap, 0);
					newSymTable += fTable;
					//println("just added " + newScope)
				}
				//println(" new scope " + newScope)

				for (c <- firstProcedure.children) {
					if (c.value == "params"){
						if(c.children.length > 0) newSymTable = addSymTableFromParams(c.children(0), newSymTable, newScope)
					}
					else {
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
				newSymTable = buildSymbolTable(c, newSymTable, inScopeOf);
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
	def printSignatures(symTable: ArrayBuffer[FunctionSymTable]) {
		for ((k, v) <- signatureMap) {
			if(k != "wain") {
				Console.err.println(k + " " + v);
				for (f<- symTable) {
					if (f._1 == k) {
						for ((k1, v1) <- f._2) {
							Console.err.println(k1 + " " + v1)
						}
					}
				}
				Console.err.println("")
			}
		}
	}
	def debugPrintSymTable(symtable: ArrayBuffer[FunctionSymTable]) {
		for (f<- symtable) {
			println("function " + f._1)
			for ((k, v) <- f._2) {
				println(k + " " + v)
			}
		}
	}
	def getSignatureMap() : Map[String, String] =  {
		return signatureMap;
	}
}