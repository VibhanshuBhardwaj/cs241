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
	var numberOfWhiles = 1;
	var input = ArrayBuffer[String]();
	
	var signatureMap = Map[String, String]();
	var FINALSYMTABLE = ArrayBuffer[FunctionSymTable]();

	type FunctionSymTable = (String, Map[String, String], Int);

	def generateCodeForTerm(term: Node) : Unit = {
		GenCodeForTerm.generate(term);
	}

	def generateCodeForFactor(factor: Node) : Unit = {
		GenCodeForFactor.generate(factor);
	}
	def generateCodeForStatements(stmts: Node, nWhile: Int) : Int = {
		val children = stmts.children;
		if (children.length == 2) {
			var newNumberOfWhiles = generateCodeForStatements(children(0), nWhile);
			var newNumberOfWhiles2 = generateCodeForONEStatement(children(1), newNumberOfWhiles);
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

 	def generateCodeForDCLs(dcls: Node) : Unit = {
		val children = dcls.children;
		if (children.length > 0) {
			generateCodeForDCLs(children(0)); //first, recursive call

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

			var offset = getValOfLexFromSymTable(lexOfDCL).split(" ")(1);
	//		println("offset from lex "  + offset);
			var inst ="";
			if (offset == "0") inst+= "sw $3, "
			else inst+= "sw $3, -"
			inst+= offset.toString;
			inst+="($29)"
			MIPSOutput.append(inst);
		}
	}
	def generateCodeForTest(test: Node) : Unit = {
		GenCodeForTerm.generate(test);
	}
	def getLexFactor(factor: Node) : String = {
		if (factor.rule == "factor ID") {
			return factor.children(0).lex;
		}
		else {
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
		else if (lvalue.rule == "LPAREN lvalue RPAREN") {
			return getLexLvalue(children(0));
		}
		else {
			MIPSOutput.append("; SHOULD NOT BE HAPPENING :( ");
			return " :( "

		}
	}
	def generateCodeForONEStatement(stmt: Node, nWhile: Int) : Int = {
		val children = stmt.children;
		if (stmt.rule.contains("PRINTLN")) {
			//println("print called")
			GenCodeForExpr.generate(children(2));
			//val push1Inst = "sw $1, -4($30)";
			//val reduceStackInst = "sub $30, $30, $4";
			val copyTo1 = "add $1, $3, $0";
			val lis10 = "lis $10";
			val printWord = ".word print"
			val callPrint = "jalr $10"
			val restore1 = "lw $1, 0($29)"
			MIPSOutput.append(copyTo1);
			MIPSOutput.append(lis10);
			MIPSOutput.append(printWord);
			MIPSOutput.append(callPrint);
			MIPSOutput.append(restore1);
			return nWhile;
		}
		else if (stmt.rule == "statement lvalue BECOMES expr SEMI") {
			//val lex = getLexLvalue(children(0));
			val lex = getLexLvalue(children(0));
			val expr = children(2);
			println("; now generating code for expr from " + stmt.rule);
			println("; lex for lvalue is " + lex)

			GenCodeForExpr.generate(expr);
			
			val offset = getValOfLexFromSymTable(lex).split(" ")(1);
			val typeOfLex =  getValOfLexFromSymTable(lex).split(" ")(0);
			var inst ="";
			if (typeOfLex == "int") {
				if (offset == "0") inst+= "sw $3, "
				else inst+= "sw $3, -"
				inst+= offset.toString;
				inst+="($29)"
				MIPSOutput.append(inst);
			}
			else if (typeOfLex == "int*") {
				MIPSOutput.append("; de-ref a pointer and assign to it")
				var push3Inst = "sw $3, -4($30)"
				var extendStackInst = "sub $30, $30, $4"

				MIPSOutput.append(push3Inst);
				MIPSOutput.append(extendStackInst);
				generateCodeForLvalue(children(0));

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
			var newNumberOfWhiles = generateCodeForWhile(stmt, nWhile);
			return newNumberOfWhiles;
		}
		else if (stmt.rule.contains("IF")) {
			var newNumberOfLabels = generateCodeForIF(stmt, nWhile);
			return newNumberOfLabels;
		}
		else if (stmt.rule.contains("DELETE")) {
			MIPSOutput.append("; generating statement -> DELETE");
			GenCodeForExpr.generate(children(3));
			//MIPSOutput.append("beq $3, $11, skipDeleteBitch")
			//MIPSOutput.append("; gen code for new expr")
			val store1 = "sw $1, -4($30)";
			val extendStackInst = "sub $30, $30, $4";
			val set1 = "add $1, $0, $3";
			MIPSOutput.append(store1);
			MIPSOutput.append(extendStackInst);
			MIPSOutput.append(set1);
			val lis10 = "lis $10";
			val newWord = ".word delete"
			val call = "jalr $10"
			MIPSOutput.append(lis10);
			MIPSOutput.append(newWord);
			MIPSOutput.append(call);
			var reduceStackInst = "add $30, $30, $4";
			var pop5Inst = "lw $1, -4($30)";
			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append(pop5Inst);
			//MIPSOutput.append("skipDeleteBitch: ")
			return nWhile;
		}
		else return nWhile;
	}
	def generateCodeForLvalue(lvalue: Node) : Unit = {

		GenCodeForLvalue.generate(lvalue);
	}
	def generateCodeForIF(stmt: Node, nLabels: Int) : Int = {
		MIPSOutput.append("; generating for if")
		val children = stmt.children;
		generateCodeForTest(children(2));
		val startLabel = "sIF" + nLabels;
		val endLabel = "eIF" + nLabels;
		val branchToTrue = "bne $3, $0, " + startLabel;
		MIPSOutput.append(branchToTrue)
		var newNumberOfLabels = generateCodeForStatements(children(9), nLabels + 1);
		val branchToEND = "beq $0, $0, " + endLabel;
		MIPSOutput.append(branchToEND);
		MIPSOutput.append(startLabel+":")
		newNumberOfLabels+= generateCodeForStatements(children(5), newNumberOfLabels + 1);
		MIPSOutput.append(endLabel+":")
		return newNumberOfLabels;

	}
	def generateCodeForWhile(stmt: Node, nWhile: Int) : Int = {
		//println("generateCodeForWhile with " + nWhile.toString)
		MIPSOutput.append("; generating code for while ")
		val children = stmt.children;
		MIPSOutput.append("sw" + nWhile.toString+ ":");

		generateCodeForTest(children(2));
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
		var newNumberOfWhiles = generateCodeForStatements(children(5), nWhile + 1);
		val goToStart1 = "lis $6";// + nWhile.toString;
		val goToStart2 = ".word sw" + nWhile.toString;
		val goToStart3 = "jr $6"
		MIPSOutput.append(goToStart1);
		MIPSOutput.append(goToStart2);
		MIPSOutput.append(goToStart3);
		MIPSOutput.append("ew" + nWhile.toString + ":");
		return newNumberOfWhiles ;
	}
	def generateCode(proceduresTree: Node) : Unit = {
		val children = proceduresTree.children;
		val mainTree = children(0);
		var size = 0;
		for (f<- FINALSYMTABLE) {
			//println("f._1 " + f._1)
			if (f._1 == "wain") {
			//	println("size " + f._2.size)
				size = f._2.size;
			}
		}
		MIPSOutput.addProlog(size*4); 
		MIPSOutput.append("; init")
		var firstParamType = Utils.getValOfLexFromSymTable(mainTree.children(3).children(1).lex);

		firstParamType = firstParamType.split(" ")(0);
		val lis10 = "lis $10";
		val initWord = ".word init"
		MIPSOutput.append(lis10);
		MIPSOutput.append(initWord);

		if (firstParamType == "int") {
			val store2 = "sw $2, -4($30)";
			val extendStackInst = "sub $30, $30, $4";
			val make2Zero = "add $2, $0, $0";
			MIPSOutput.append(store2);
			MIPSOutput.append(extendStackInst);
			MIPSOutput.append(make2Zero)
		}
		val callInit = "jalr $10"
		MIPSOutput.append(callInit);
		if (firstParamType == "int") {
			var reduceStackInst = "add $30, $30, $4";
			var pop2Inst = "lw $2, -4($30)";
			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append(pop2Inst);
		}
		val expr = mainTree.children(11);
		val stmts = mainTree.children(9);
		val dcls = mainTree.children(8);

		generateCodeForDCLs(dcls);
		generateCodeForStatements(stmts, numberOfWhiles);
		GenCodeForExpr.generate(expr);

		//val id = expr.children(0).children(0);
		//val lex = id.lex;
		
		
		MIPSOutput.addEpilog(size*4);
		MIPSOutput.printOutput();
		
	}
	def main(args: Array[String]) : Unit = {
		var ParseTree = new Node("ROOT", "");
		ParseTree = ParseTreeBuilder.construct(ParseTree).children(0);

		var symTable = ArrayBuffer[FunctionSymTable]();
		symTable = SymbolTableBuilder.buildSymbolTable(ParseTree, symTable, "");
		val procedures = ParseTree.children(1);
		FINALSYMTABLE = symTable;
		Utils.init(FINALSYMTABLE)
		//SymbolTableBuilder.debugPrintSymTable(FINALSYMTABLE)
		//generateCodeForStatements()
		generateCode(procedures);
		//SymbolTableBuilder.debugPrintSymTable(symTable);
		

	}

}