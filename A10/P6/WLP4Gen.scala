import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map; //[String, Int]()

import gen.Node;

import MIPSOutput._;
import TypeChecker._;
import SymbolTableBuilder._;
import ParseTreeBuilder._;

import GenCodeForExpr._;
import GenCodeForTerm._;
import GenCodeForFactor._;
import GenCodeForTest._;
import GenCodeForLvalue._;
import Utils._;

object WLP4Gen {
	var ParseTree = new Node("ROOT", "");
	var numberOfWhiles = 1;
	var input = ArrayBuffer[String]();
	
	var signatureMap = Map[String, String]();
	var FINALSYMTABLE = ArrayBuffer[FunctionSymTable]();

	type FunctionSymTable = (String, Map[String, String], Int);
	var functionsUsed = Set[String]();

	// def generateCodeForTerm(term: Node) : Unit = {
	// 	GenCodeForTerm.generate(term);
	// }
	def generateCodeForFunction(id: String, procedures: Node) : Unit = {
		val rule = procedures.rule;
		val children = procedures.children;
		if (rule == "procedures main") {
			//handle wain?
			println("; should this be happening rn")
		}
		else if (rule == "procedures procedure procedures") {
			if (children(0).lex == id) {
				//genCodeForFunction
			}
			else generateCodeForFunction(id, children(1));
		}

	}
	// def generateCodeForFactor(factor: Node) : Unit = {
	// 	if (factor.rule == "factor ID LPAREN RPAREN") {
	// 		val procedures = ParseTree.children(1);
	// 		generateCodeForFunction(factor.children(0).lex, procedures);
	// 	}
	// 	else { 
	// 		GenCodeForFactor.generate(factor);
	// 	}
	// }
	def generateCodeForStatements(stmts: Node, nWhile: Int, name: String) : Int = {
		val children = stmts.children;
		if (children.length == 2) {
			var newNumberOfWhiles = generateCodeForStatements(children(0), nWhile, name);
			var newNumberOfWhiles2 = generateCodeForONEStatement(children(1), newNumberOfWhiles, name);
			return newNumberOfWhiles2;
		}
		else return nWhile;
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

 	def generateCodeForDCLs(dcls: Node, name: String) : Unit = {
		val children = dcls.children;
		if (children.length > 0) {
			generateCodeForDCLs(children(0), name); //first, recursive call

			//then gen code for curr dcl.
			val typeOfDCL = getTypeDCL(children(1));
			val lexOfDCL = children(1).children(1).lex;
			if (dcls.rule.contains("NUM")) { 
				val num = children(3).lex;
				//val lex = 
				val lis3 = "lis $3";
				val dotWord = ".word " + num;
				MIPSOutput.append(lis3);
				MIPSOutput.append(dotWord);
				
			}
			else if (dcls.rule.contains("NULL")) {
				val set3ToNull = "add $3, $0, $11";
				MIPSOutput.append(set3ToNull)
			}

			var offset = getValOfLexFromSymTable(lexOfDCL, name).split(" ")(1);
	//		println("offset from lex "  + offset);
			var inst ="";
			if (offset == "0") inst+= "sw $3, "
			else inst+= "sw $3, -"
			inst+= offset.toString;
			inst+="($29)"
			MIPSOutput.append(inst);
		}
	}
	def generateCodeForTest(test: Node, funcName: String) : Unit = {
		GenCodeForTest.generate(test, funcName);
	}
	def getLexFactor(factor: Node) : String = {
		if (factor.rule == "factor ID") {
			return factor.children(0).lex;
		}
		else {
			println(";fuck up :) ")
			MIPSOutput.append("; SHOULD NOT BE HAPPENING :( ");
			return " : ( "
		}
	}
	def getLexLvalue(lvalue: Node) : String = {
		val children =  lvalue.children;
		if (lvalue.rule == "lvalue ID") {
			val lex= children(0).lex;
			return lex;
		}
		else if (lvalue.rule == "lvalue STAR factor") {
			return getLexFactor(children(1));
		}
		else if (lvalue.rule == "lvalue LPAREN lvalue RPAREN") {
			return getLexLvalue(children(1));
		}
		else {
			MIPSOutput.append("; SHOULD NOT BE HAPPENING :( ");
			return " :( "

		}
	}
	def generateCodeForONEStatement(stmt: Node, nWhile: Int, name: String) : Int = {
		val children = stmt.children;
		if (stmt.rule.contains("PRINTLN")) {
			//println("print called")
			GenCodeForExpr.generate(children(2), name);
			val push1Inst = "sw $1, -4($30)";
			val extendStackInst = "sub $30, $30, $4";
			MIPSOutput.append(push1Inst);
			MIPSOutput.append(extendStackInst);
			//MIPSOutput.append("sw $31, -4($30)")
			//MIPSOutput.append(extendStackInst);

			val copyTo1 = "add $1, $3, $0";
			MIPSOutput.append(copyTo1);
			val lis10 = "lis $10";
			val printWord = ".word print"
			val callPrint = "jalr $10"
			MIPSOutput.append(lis10);
			MIPSOutput.append(printWord);
			MIPSOutput.append(callPrint);

			var reduceStackInst = "add $30, $30, $4";
		//	MIPSOutput.append(reduceStackInst);
		//	MIPSOutput.append("lw $31, -4($30)");
			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append("lw $1, -4($30)")
			
			return nWhile;
		}
		else if (stmt.rule == "statement lvalue BECOMES expr SEMI") {
			//
			
			val expr = children(2);
			val lvalue = children(0);
			//println("; now generating code for expr from " + stmt.rule);
			GenCodeForExpr.generate(expr, name);
			
			//
			//val typeOfLex =  TypeChecker.checkTypes(children(0), FINALSYMTABLE, "wain");
			//println("typeOfLex : " +typeOfLex)
			var inst ="";
			if (!lvalue.rule.contains("STAR")) {
				val lex = getLexLvalue(children(0));
				//println("lex is "  + lex)
				val offset = getValOfLexFromSymTable(lex, name).split(" ")(1);
				//println("offset is " + offset)
				if (offset == "0") inst+= "sw $3, "
				else inst+= "sw $3, -"
				inst+= offset.toString;
				inst+="($29)"
				MIPSOutput.append(inst);
			}
			else {
				MIPSOutput.append("; de-ref a pointer and assign to it")
				var push3Inst = "sw $3, -4($30)"
				var extendStackInst = "sub $30, $30, $4"

				MIPSOutput.append(push3Inst);
				MIPSOutput.append(extendStackInst);
				generateCodeForLvalue(children(0), name);

				var reduceStackInst = "add $30, $30, $4";
				var pop5Inst = "lw $5, -4($30)";

				MIPSOutput.append(reduceStackInst);
				MIPSOutput.append(pop5Inst);
				val storeDereferencedVal = "sw $5, 0($3)"
				MIPSOutput.append(storeDereferencedVal);

			}
			return nWhile;
		}
		else if (stmt.rule.contains("WHILE")) {
			var newNumberOfWhiles = generateCodeForWhile(stmt, nWhile, name);
			return newNumberOfWhiles;
		}
		else if (stmt.rule.contains("IF")) {
			var newNumberOfLabels = generateCodeForIF(stmt, nWhile, name);
			return newNumberOfLabels;
		}
		else if (stmt.rule.contains("DELETE")) {
			MIPSOutput.append("; generating statement -> DELETE");
			GenCodeForExpr.generate(children(3), name);
			MIPSOutput.append("beq $3, $11, skipDeleteBitch" + nWhile)
			//MIPSOutput.append("; gen code for new expr")
			val store1 = "sw $1, -4($30)";
			val extendStackInst = "sub $30, $30, $4";
			val set1 = "add $1, $0, $3";
			MIPSOutput.append(store1);
			MIPSOutput.append(extendStackInst);
			MIPSOutput.append(set1);

			//MIPSOutput.append("sw $31, -4($30)")
			//MIPSOutput.append(extendStackInst);

			val lis10 = "lis $10";
			val newWord = ".word delete"
			val call = "jalr $10"
			MIPSOutput.append(lis10);
			MIPSOutput.append(newWord);
			MIPSOutput.append(call);
			var reduceStackInst = "add $30, $30, $4";
			var pop5Inst = "lw $1, -4($30)";
			//MIPSOutput.append(reduceStackInst);
			//MIPSOutput.append("lw $31, -4($30)")
			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append(pop5Inst);
			MIPSOutput.append("skipDeleteBitch" + nWhile+ ":")
			return nWhile + 1;
		}
		else return nWhile;
	}
	def generateCodeForLvalue(lvalue: Node, funcName: String) : Unit = {

		GenCodeForLvalue.generate(lvalue, funcName);
	}
	def generateCodeForIF(stmt: Node, nLabels: Int, funcName: String) : Int = {
		MIPSOutput.append("; generating for if")
		val children = stmt.children;
		generateCodeForTest(children(2), funcName);
		val startLabel = "sIF" + nLabels;
		val endLabel = "eIF" + nLabels;
		val branchToTrue = "bne $3, $0, " + startLabel;
		MIPSOutput.append(branchToTrue)
		var newNumberOfLabels = generateCodeForStatements(children(9), nLabels + 1, funcName);
		val branchToEND = "beq $0, $0, " + endLabel;
		MIPSOutput.append(branchToEND);
		MIPSOutput.append(startLabel+":")
		newNumberOfLabels+= generateCodeForStatements(children(5), newNumberOfLabels + 1, funcName);
		MIPSOutput.append(endLabel+":")
		return newNumberOfLabels;

	}
	def generateCodeForWhile(stmt: Node, nWhile: Int, funcName: String) : Int = {
		//println("generateCodeForWhile with " + nWhile.toString)
		MIPSOutput.append("; generating code for while ")
		val children = stmt.children;
		MIPSOutput.append("sw" + nWhile.toString+ ":");

		generateCodeForTest(children(2), funcName);
		val branchToEnd = "beq $3, $0, 1" //+ nWhile.toString;
		val temp = "beq $3, $11, 3"
		val t1= "lis $6"
		val t2 = ".word ew" + nWhile.toString;
		val t3 = "jr $6";
		MIPSOutput.append(branchToEnd);
		MIPSOutput.append(temp);
		MIPSOutput.append(t1);
		MIPSOutput.append(t2);
		MIPSOutput.append(t3);
		var newNumberOfWhiles = generateCodeForStatements(children(5), nWhile + 1, funcName);
		val goToStart1 = "lis $6";// + nWhile.toString;
		val goToStart2 = ".word sw" + nWhile.toString;
		val goToStart3 = "jr $6"
		MIPSOutput.append(goToStart1);
		MIPSOutput.append(goToStart2);
		MIPSOutput.append(goToStart3);
		MIPSOutput.append("ew" + nWhile.toString + ":");
		return newNumberOfWhiles ;
	}
	def generateCodeForWain(main: Node) : Unit = {
		val currProcedure = main;
		var size = 0;
		for (f<- FINALSYMTABLE) {
			//println("f._1 " + f._1)
			if (f._1 == "wain") {
			//	println("size " + f._2.size)
				size = f._2.size;
			}
		}
		MIPSOutput.append("F" + "wain" + ":")
		MIPSOutput.addProlog(size*4, "wain"); 
		MIPSOutput.append("; init")
		var firstParamType = Utils.getValOfLexFromSymTable(currProcedure.children(3).children(1).lex, "wain");

		firstParamType = firstParamType.split(" ")(0);
		val lis10 = "lis $10";
		val initWord = ".word init"
		MIPSOutput.append(lis10);
		MIPSOutput.append(initWord);
		val extendStackInst = "sub $30, $30, $4";
		if (firstParamType == "int") {
			val store2 = "sw $2, -4($30)";
			
			val make2Zero = "add $2, $0, $0";
			MIPSOutput.append(store2);
			MIPSOutput.append(extendStackInst);
			MIPSOutput.append(make2Zero)
		}

		//MIPSOutput.append("sw $31, -4($30)")
		//MIPSOutput.append(extendStackInst)
		val callInit = "jalr $10"
		MIPSOutput.append(callInit);
		var reduceStackInst = "add $30, $30, $4";
		//MIPSOutput.append(reduceStackInst);
		//MIPSOutput.append("lw $31, -4($30)")
		if (firstParamType == "int") {
			
			var pop2Inst = "lw $2, -4($30)";
			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append(pop2Inst);
		}

		val expr = currProcedure.children(11);
		val stmts = currProcedure.children(9);
		val dcls = currProcedure.children(8);

		generateCodeForDCLs(dcls, "wain");
		numberOfWhiles = numberOfWhiles + generateCodeForStatements(stmts, numberOfWhiles, "wain");
		GenCodeForExpr.generate(expr, "wain");

		//val id = expr.children(0).children(0);
		//val lex = id.lex;
		
		
		MIPSOutput.addEpilog(size*4, "wain");
	}
	def generateCodeForProcedure(procedure: Node) : Unit = {
		val children = procedure.children;
		val name = children(1).lex;
		if (true) { //function is used somewhere
			MIPSOutput.append("F" + name + ":")
			var size = 0;
			for (f<- FINALSYMTABLE) {
			
				if (f._1 == name) {
			
					size = f._2.size;
				}
			}
			MIPSOutput.addProlog(size*4, name);
			val expr = children(9);
			val stmts = children(7);
			val dcls = children(6);

			generateCodeForDCLs(dcls, name);
			numberOfWhiles = numberOfWhiles + generateCodeForStatements(stmts, numberOfWhiles, name);
			GenCodeForExpr.generate(expr, name);
			MIPSOutput.addEpilog(size*4, name)
		}
	}
	def generateCode(proceduresTree: Node) : Unit = {
		val children = proceduresTree.children;
		if (proceduresTree.rule == "procedures main") {
			generateCodeForWain(children(0));
			MIPSOutput.printOutput();
		}
		else {
			generateCodeForProcedure(children(0));
			generateCode(children(1));
		}
	}
	def populateFunctionsUsed (tree: Node) {
		if (tree.rule.startsWith("factor ID LPAREN")) {
			functionsUsed += tree.children(0).lex;
		}
		else {
			for (c<- tree.children) {
				populateFunctionsUsed(c);
			}
		}
	}
	def populateFunctionsUsedProcedures(procedures: Node) {
		val children = procedures.children;
		if (procedures.rule.contains("main")) {
			val MainChildren = children(0).children;
			populateFunctionsUsed(MainChildren(9));
			populateFunctionsUsed(MainChildren(11));
		}
		else {

			populateFunctionsUsedProcedures(children(1));
			val nameCurr = children(0).children(1).lex;
			if (functionsUsed(nameCurr)) {
				populateFunctionsUsed(children(0).children(7));
				populateFunctionsUsed(children(0).children(9));
			}
		}

	}
	def populateFunctionsUsedInWain(tree: Node) {
		
	}
	def main(args: Array[String]) : Unit = {
		
		ParseTree = ParseTreeBuilder.construct(ParseTree).children(0);

		var symTable = ArrayBuffer[FunctionSymTable]();
		symTable = SymbolTableBuilder.buildSymbolTable(ParseTree, symTable, "");
		
		FINALSYMTABLE = symTable;
		//populateFunctionsUsedInWain(ParseTree);
		val procedures = ParseTree.children(1);

		//populateFunctionsUsedProcedures(procedures);

		//for (f<- functionsUsed) {
		//	println(";have used " + f);
		//Ffuncti}
		signatureMap = SymbolTableBuilder.getSignatureMap();
		TypeChecker.setup(signatureMap, signatureMap, FINALSYMTABLE)
		MIPSOutput.init();
		Utils.init(FINALSYMTABLE);
		GenCodeForExpr.init(FINALSYMTABLE);
		GenCodeForTest.init(FINALSYMTABLE);

		//SymbolTableBuilder.debugPrintSymTable(FINALSYMTABLE)
		//generateCodeForStatements()
		generateCode(procedures);
		//SymbolTableBuilder.debugPrintSymTable(symTable);
		

	}

}