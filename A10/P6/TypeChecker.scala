//package gen
import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map;

import gen.Node;

object TypeChecker {
	type FunctionSymTable = (String, Map[String, String], Int);
	var signatureMap = Map[String, String]();
	var paramMap = Map[String, String]();
	var FINALSYMTABLE = ArrayBuffer[FunctionSymTable]();

	def raiseError(message: String) : String = {
		Console.err.println("ERROR: " + message);
		System.exit(1);
		return "exiting";
	}
	def setup(sm: Map[String, String], pm: Map[String, String], symTable: ArrayBuffer[FunctionSymTable]) {
		signatureMap = sm;
		paramMap = pm;
		FINALSYMTABLE = symTable;
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
	def checkTypesProcedures(procedures: Node) {
		//println("checking types");
		
		if (procedures.children.length == 1) { //
			val mainTree = procedures.children(0);
			val secondDCLtype = getTypeDCL(mainTree.children(5));
			if (secondDCLtype != "int") {
				Console.err.println("ERROR: second param of wain must be int")
				System.exit(1);
			}
			TypeChecker.checkTypes(mainTree.children(8), FINALSYMTABLE, "wain");
			checkTypesStatements(mainTree.children(9), "wain");

			val retType= TypeChecker.checkTypes(mainTree.children(11), FINALSYMTABLE, "wain");
			if (retType != "int") {
				Console.err.println("ERROR: returning a non int value")
				System.exit(1);
			}
		}
		else {
			checkTypesONEProcedure(procedures.children(0));
			checkTypesProcedures(procedures.children(1));
		}
	}
	def checkTypesONEProcedure(procedure: Node) {
		TypeChecker.checkTypes(procedure.children(6), FINALSYMTABLE, procedure.children(1).lex);
		checkTypesStatements(procedure.children(7), procedure.children(1).lex);
		
		val retType = TypeChecker.checkTypes(procedure.children(9), FINALSYMTABLE, procedure.children(1).lex);
		if (retType != "int") {
			Console.err.println("ERROR: returning a non int value")
			System.exit(1);
		}
	}
	def checkTypeOneStatement(statement: Node, scope: String) {
		if(statement.rule == "statement lvalue BECOMES expr SEMI") {
			val Exp1 = TypeChecker.checkTypes(statement.children(0), FINALSYMTABLE, scope);
			val Exp2 = TypeChecker.checkTypes(statement.children(2), FINALSYMTABLE, scope);
			if (Exp1 != Exp2) {
				Console.err.println("ERROR: assigning " + Exp1 + " to " + Exp2);
				System.exit(1);
			}
		}
		else if (statement.rule.contains("PRINTLN")) {
			val Exp1 = TypeChecker.checkTypes(statement.children(2), FINALSYMTABLE, scope);
			if (Exp1 != "int") {
				Console.err.println("ERROR: printing non int value");
				System.exit(1);
			}
		}
		else if (statement.rule.contains("DELETE")) {
			val Exp1 = TypeChecker.checkTypes(statement.children(3), FINALSYMTABLE, scope);
			if (Exp1 != "int*") {
				Console.err.println("ERROR: deleting a non ptr value");
				System.exit(1);
			}
		}
		for (c<- statement.children) {
			if (c.value == "statements") checkTypesStatements(c, scope);
			else if (c.value == "expr") TypeChecker.checkTypes(c, FINALSYMTABLE, scope);
			else if (c.value == "test") TypeChecker.checkTypes(c, FINALSYMTABLE, scope);
		}
	}
	def checkTypesStatements(statements: Node, scope: String) {
		for (c<- statements.children) {
			if (c.value == "statement") checkTypeOneStatement(c, scope);
			else checkTypesStatements(c, scope);
		}
	}
	def checkTypes(tree: Node, symTable: ArrayBuffer[FunctionSymTable], scope: String) : String = {

		val rule = tree.rule;
		val value = tree.value;
		val lex = tree.lex;
		val children = tree.children;
		if (value == "ID")  {
			var thisFuncSymTable = Map[String, String]();
			for (ft<- symTable) {
				if (ft._1 == scope) {
					thisFuncSymTable = ft._2;						}
				}
			//if (fTable == None) raiseError("ERROR: SHOULD NOT BE HAPPENING");
			if (thisFuncSymTable.contains(lex)) return thisFuncSymTable(lex).split(" ")(0);
			else return raiseError("ERROR: SHOULD NOT BE HAPPENING")
		}
		// else if (rule == "procedures main") {

		// }
		// else if (value == "procedures")
		else if (rule == "expr term") {
			return checkTypes(children(0), symTable, scope);
		}
		else if (rule.startsWith("dcls ")) {
			//println("startsWith dcls")
			if (children.length == 0) return "";
			val dclsType = checkTypes(children(0), symTable, scope);
			if (rule.contains("NUM")) {
				var declaredType = checkTypes(children(1).children(1), symTable, scope);
				if (declaredType == "int*") return raiseError("ERROR: assigning int to ptr");
			}
			else if (rule.contains("NULL")) {
				var declaredType = checkTypes(children(1).children(1), symTable, scope);
				//println("in NULL declaredType " + declaredType)
				if (declaredType == "int") return raiseError("ERROR: assigning NULL to integer");
			}
			return "";
		}
		else if (rule == "expr expr PLUS term") {
			val firstE = checkTypes(children(0), symTable, scope);
			val secondE =  checkTypes(children(2), symTable, scope);
			if (firstE == "int" && secondE=="int") return "int";
			else if (firstE == "int*" && secondE=="int") return "int*";
			else if (firstE == "int" && secondE=="int*") return "int*";
			else return raiseError("ERROR: ADDING TWO POINTERS IS NOT ALLOWED")
		}
		else if (rule == "expr expr MINUS term") {
			val firstE = checkTypes(children(0), symTable, scope);
			val secondE =  checkTypes(children(2), symTable, scope);
			if (firstE == "int" && secondE=="int") return "int";
			else if (firstE == "int*" && secondE=="int") return "int*";
			else if (firstE == "int*" && secondE=="int*") return "int";
			else return raiseError("ERROR: ADDING TWO POINTERS IS NOT ALLOWED");
		}
		else if (rule == "term factor") {
			return checkTypes(children(0), symTable, scope);
		}
		else if (rule == "term term STAR factor" || rule == "term term SLASH factor" || rule == "term term PCT factor") {
			val firstE = checkTypes(children(0), symTable, scope);
			val secondE = checkTypes(children(2), symTable, scope);
			if (firstE == "int" && secondE == "int") return "int";
			else return raiseError("ERROR: *, /, % are only defined for 2 ints");
		}
		else if (rule == "factor ID") {
			return checkTypes(children(0), symTable, scope);
		}
		else if (rule == "factor NUM") {
			return "int";
		}
		else if (rule == "factor LPAREN expr RPAREN") {
			return checkTypes(children(1), symTable, scope);
		}
		else if (rule == "factor AMP lvalue") {
			val firstE = checkTypes(children(1), symTable, scope);
			if (firstE =="int") return "int*";
			else return raiseError("ERROR: tried addressing a non integer value");
		}
		else if (rule == "factor STAR factor") {
			val firstE = checkTypes(children(1), symTable, scope);
			if (firstE == "int*") return "int";
			else return raiseError("ERROR: tried derefencing a non-pointer value");
		}
		else if (rule == "factor NEW INT LBRACK expr RBRACK") {
			val firstE = checkTypes(children(3), symTable, scope);
			if (firstE == "int") return "int*";
			else return raiseError("ERROR: new int[] must have int inside")
		}
		else if (rule == "factor ID LPAREN RPAREN") {
			return "int";
		}
		else if (rule == "factor ID LPAREN arglist RPAREN") {
			return "int"
			// val argTypes = checkArglistTypes(children(2), symTable, scope, "");
			// val functionName = children(0).lex;
			// println("paramMap for " + functionName + " : " + paramMap(functionName));
			// println("argTypes: " + argTypes);
			// if (paramMap(functionName) == argTypes) return "int";
			// else return raiseError("ERROR: function " + functionName + " called with incorrect arguments");
		}
		else if (rule == "lvalue ID") {
			return checkTypes(children(0), symTable, scope);
		}
		else if (rule == "lvalue STAR factor") {
			val firstE = checkTypes(children(1), symTable, scope);
			if (firstE == "int*") return "int";
			else return raiseError("ERROR: tried derefencing a non-pointer value");
		}
		else if (rule == "lvalue LPAREN lvalue RPAREN") {
			return checkTypes(children(1), symTable, scope);
		}
		else if (rule.startsWith("test ")) {
			val firstE = checkTypes(children(0), symTable, scope);
			val secondE = checkTypes(children(2), symTable, scope);
			if (firstE == secondE) return "";
			else return raiseError("comparing 2 different types")
		}
		else return "VALID- SHOULD THIS BE HAPPENING?"
	}
	def checkArglistTypes(arglist: Node, symTable: ArrayBuffer[FunctionSymTable], scope: String, curr:String) : String = {
		//var type = "";
		var children = arglist.children;
		if (arglist.rule == "arglist expr") {
			if (curr=="") return checkTypes(children(0), symTable, scope);
			else return curr + " " + checkTypes(children(0), symTable, scope);
		}
		else if (arglist.rule == "arglist expr COMMA arglist") {
			val exprType = checkTypes(children(0), symTable, scope);
			var newCurr = curr + " " + exprType;
			return checkArglistTypes(children(2), symTable, scope, newCurr);
		}
		else return ""
	}
	def main(args: Array[String]) : Unit = {

	}
}