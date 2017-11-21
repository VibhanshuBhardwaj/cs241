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
	var numberOfWhiles = 1;
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

		else if (expr.rule == "expr expr PLUS term") {
			MIPSOutput.append("; expr -> expr PLUS term");
			generateCodeForExpr(children(0));

			var push3Inst = "sw $3, -4($30)"
			var extendStackInst = "sub $30, $30, $4"

			MIPSOutput.append(push3Inst);
			MIPSOutput.append(extendStackInst);

			generateCodeForTerm(children(2));

			var reduceStackInst = "add $30, $30, $4";
			var pop5Inst = "lw $5, -4($30)";

			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append(pop5Inst);
			var finalAddInst = "add $3, $5, $3";
			MIPSOutput.append(finalAddInst);

		}

		else if (expr.rule == "expr expr MINUS term") {
			MIPSOutput.append("; expr -> expr MINUS term")
			generateCodeForExpr(children(0));

			var push3Inst = "sw $3, -4($30)"
			var extendStackInst = "sub $30, $30, $4"

			MIPSOutput.append(push3Inst);
			MIPSOutput.append(extendStackInst);

			generateCodeForTerm(children(2));

			var reduceStackInst = "add $30, $30, $4";
			var pop5Inst = "lw $5, -4($30)";

			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append(pop5Inst);
			var finalAddInst = "sub $3, $5, $3";
			MIPSOutput.append(finalAddInst);
			
		}
	}
	def generateCodeForTerm(term: Node) : Unit = {
		val children = term.children;
		val rule = term.rule;
		if (rule == "term factor") {
			//println("for factor called with " + children(0).value)
			generateCodeForFactor(children(0));
		}
		else if (rule == "term term STAR factor") {
			MIPSOutput.append("; term -> term STAR factor code starts");
			generateCodeForTerm(children(0));
			var push3Inst = "sw $3, -4($30)"
			var extendStackInst = "sub $30, $30, $4"

			MIPSOutput.append(push3Inst);
			MIPSOutput.append(extendStackInst);

			generateCodeForFactor(children(2));
			var reduceStackInst = "add $30, $30, $4";
			var pop5Inst = "lw $5, -4($30)";

			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append(pop5Inst);
			val multiplyThem = "mult $3, $5";
			val mflo = "mflo $3";
			MIPSOutput.append(multiplyThem);
			MIPSOutput.append(mflo) 
			MIPSOutput.append("; term -> term STAR factor code ends")


		}
		else if (rule == "term term SLASH factor") {
			MIPSOutput.append("; term -> term SLASH factor code starts");
			generateCodeForTerm(children(0));
			var push3Inst = "sw $3, -4($30)"
			var extendStackInst = "sub $30, $30, $4"

			MIPSOutput.append(push3Inst);
			MIPSOutput.append(extendStackInst);

			generateCodeForFactor(children(2));
			var reduceStackInst = "add $30, $30, $4";
			var pop5Inst = "lw $5, -4($30)";

			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append(pop5Inst);
			val divThem = "div $5, $3";
			val mflo = "mflo $3";
			MIPSOutput.append(divThem);
			MIPSOutput.append(mflo) 
			MIPSOutput.append("; term -> term SLASH factor code ends")
		}
		else if (rule == "term term PCT factor") {
			MIPSOutput.append("; term -> term PCT factor code starts");
			generateCodeForTerm(children(0));
			var push3Inst = "sw $3, -4($30)"
			var extendStackInst = "sub $30, $30, $4"

			MIPSOutput.append(push3Inst);
			MIPSOutput.append(extendStackInst);

			generateCodeForFactor(children(2));
			var reduceStackInst = "add $30, $30, $4";
			var pop5Inst = "lw $5, -4($30)";

			MIPSOutput.append(reduceStackInst);
			MIPSOutput.append(pop5Inst);
			val divThem = "div $5, $3";
			val mflo = "mfhi $3";
			MIPSOutput.append(divThem);
			MIPSOutput.append(mflo) 
			MIPSOutput.append("; term -> term PCT factor code ends")

		}
	}
	def generateCodeForFactor(factor: Node) : Unit = {
		val children = factor.children;
		val rule = factor.rule
		if (rule == "factor LPAREN expr RPAREN") {
			//println("for expr called with " +children(1).value)
			generateCodeForExpr(children(1));
		}
		else if (rule == "factor NUM") {
			val num = children(0);
			val lex = num.lex;
			val loadWordTo3 = "lis $3";
			var constToBeLoaded = ".word " + lex;
			MIPSOutput.append(loadWordTo3);
			MIPSOutput.append(constToBeLoaded);
		}

		else if (rule == "factor ID") {
			val id = children(0);
			val lex = id.lex;
			for (f <- FINALSYMTABLE) {
		
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
	def generateCodeForStatements(stmts: Node, nWhile: Int) : Int = {
		val children = stmts.children;
		if (children.length == 2) {
			var newNumberOfWhiles = generateCodeForStatements(children(0), nWhile);
			var newNumberOfWhiles2 = generateCodeForONEStatement(children(1), newNumberOfWhiles);
			return newNumberOfWhiles2;
		}
		else return nWhile;
	}
	def generateCodeForDCLs(dcls: Node) : Unit = {
		val children = dcls.children;
		if (children.length > 0) {
			generateCodeForDCLs(children(0));
			val num = children(3).lex;
			val lex = children(1).children(1).lex;
			val lis3 = "lis $3";
			val dotWord = ".word " + num;
			MIPSOutput.append(lis3);
			MIPSOutput.append(dotWord);
			for (f <- FINALSYMTABLE) {
				if (f._1 == "wain") {
						var offset = f._2(lex).split(" ")(1);
				//		println("offset from lex "  + offset);
						var inst ="";
						if (offset == "0") inst+= "sw $3, "
						else inst+= "sw $3, -"
						inst+= offset.toString;
						inst+="($29)"
						MIPSOutput.append(inst);
				}
			}
		}
	}
	def generateCodeForTest(test: Node) : Unit = {
		val children = test.children;
		val exp1 = children(0);
		val exp2 = children(2);
		generateCodeForExpr(exp1);
		var push3Inst = "sw $3, -4($30)"
		var extendStackInst = "sub $30, $30, $4"

		MIPSOutput.append(push3Inst);
		MIPSOutput.append(extendStackInst);
		generateCodeForExpr(exp2);
		var reduceStackInst = "add $30, $30, $4";
		var pop5Inst = "lw $5, -4($30)";
		MIPSOutput.append(reduceStackInst);
		MIPSOutput.append(pop5Inst)
		
		if (children(1).value == "LT") {
			MIPSOutput.append("; LT code")
			val sltInst = "slt $3, $5, $3";
			MIPSOutput.append(sltInst);
		}
		else if (children(1).value == "GT") {
			MIPSOutput.append("; GT code")
			val sltInst = "slt $3, $3, $5";
			MIPSOutput.append(sltInst);
		}
		else if (children(1).value == "GE") {
			MIPSOutput.append("; GE code. inverting LT code")
			val sltInst = "slt $3, $5, $3";
			val not3 = "sub $3, $11, $3";
			MIPSOutput.append(sltInst);
			MIPSOutput.append(not3);
		}
		else if (children(1).value == "LE") {
			MIPSOutput.append("; LE code. inverting GT code")
			val sltInst = "slt $3, $3, $5";
			val not3 = "sub $3, $11, $3";
			MIPSOutput.append(sltInst);
			MIPSOutput.append(not3);
		}
		else if (children(1).value == "NE") {
			MIPSOutput.append("; NE code");
			val sltInst1 = "slt $6, $3, $5";
			val sltInst2 = "slt $7, $5, $3";
			val add = "add $3, $6, $7";
			MIPSOutput.append(sltInst1);
			MIPSOutput.append(sltInst2);
			MIPSOutput.append(add);
		}
		else if (children(1).value == "EQ") {
			MIPSOutput.append("; EQ code. inverting NE");
			val sltInst1 = "slt $6, $3, $5";
			val sltInst2 = "slt $7, $5, $3";
			val add = "add $3, $6, $7";
			val not3 = "sub $3, $11, $3";
			MIPSOutput.append(sltInst1);
			MIPSOutput.append(sltInst2);
			MIPSOutput.append(add);
			MIPSOutput.append(not3);

		}
	}
	def getLexLvalue(lvalue: Node) : String = {
		if (lvalue.children.length == 1) {
			val lex= lvalue.children(0).lex;
			return lex;
		}
		else return getLexLvalue(lvalue.children(1));
	}
	def generateCodeForONEStatement(stmt: Node, nWhile: Int) : Int = {
		val children = stmt.children;
		if (stmt.rule.contains("PRINTLN")) {
			//println("print called")
			generateCodeForExpr(children(2));
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
			val lex = getLexLvalue(children(0));
			val expr = children(2);
			generateCodeForExpr(expr);
			
			for (f <- FINALSYMTABLE) {
				if (f._1 == "wain") {
					val offset = f._2(lex).split(" ")(1);
					var inst ="";
					if (offset == "0") inst+= "sw $3, "
					else inst+= "sw $3, -"
					inst+= offset.toString;
					inst+="($29)"
					MIPSOutput.append(inst);
				}
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
		else return nWhile;
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
		MIPSOutput.addProlog(size*4); //replace with actual size...
		//println("main tree? " + mainTree.value + " children len "  + mainTree.children.length)
		val expr = mainTree.children(11);
		val stmts = mainTree.children(9);
		val dcls = mainTree.children(8);
		generateCodeForDCLs(dcls);
		generateCodeForStatements(stmts, numberOfWhiles);
		generateCodeForExpr(expr);
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
		//SymbolTableBuilder.debugPrintSymTable(FINALSYMTABLE)
		//generateCodeForStatements()
		generateCode(procedures);
		//SymbolTableBuilder.debugPrintSymTable(symTable);
		

	}

}