//package gen
import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map;

import gen.Node;

object TypeChecker {
	type FunctionSymTable = (String, Map[String, String]);
	var signatureMap = Map[String, String]();
	var paramMap = Map[String, String]();

	def raiseError(message: String) : String = {
		Console.err.println("ERROR: " + message);
		System.exit(1);
		return "exiting";
	}
	def setup(sm: Map[String, String], pm: Map[String, String]) {
		signatureMap = sm;
		paramMap = pm;
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
			if (thisFuncSymTable.contains(lex)) return thisFuncSymTable(lex);
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
			val argTypes = checkArglistTypes(children(2), symTable, scope, "");
			val functionName = children(0).lex;
			//println("paramMap for " + functionName + " : " + paramMap(functionName));
			//println("argTypes: " + argTypes);
			if (paramMap(functionName) == argTypes) return "int";
			else return raiseError("ERROR: function " + functionName + " called with incorrect arguments");
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