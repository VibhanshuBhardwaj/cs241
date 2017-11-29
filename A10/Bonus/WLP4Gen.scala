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
	var varsUsed = Map[String, Set[String]]();
	val availableRegisters = Array("8", "9", "10", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28");
	var unAddressedUsedVars = Map[String, Int]();
	var MappingToRegisters = Map[String, String]();
	
	var constMapping = Map[String, String]();

	def populateVarCountsProcedures(procedures: Node) {
		val children = procedures.children;
		if (procedures.rule.contains("main")) {
			val MainChildren = children(0).children;
			populateVarCounts(MainChildren(9), "wain");
			populateVarCounts(MainChildren(11), "wain");
		}
		else {

			populateVarCountsProcedures(children(1));
			val nameCurr = children(0).children(1).lex;
			if (functionsUsed(nameCurr)) {
				populateVarCounts(children(0).children(7), nameCurr);
				populateVarCounts(children(0).children(9), nameCurr);
			}
		}
	}
	def populateVarCounts(tree: Node, funcName: String) {
		val children = tree.children;
		val rule = tree.rule;
		if (rule == "factor ID") {
			val lex = children(0).lex;
			val fullName = funcName + " " + lex;
			if ( (varsUsed(funcName) contains lex) && !(constMapping contains fullName)) { //FLAG 
				
				if (unAddressedUsedVars contains fullName) {unAddressedUsedVars(fullName) = unAddressedUsedVars(fullName) + 1;}
				else {
					unAddressedUsedVars += (fullName -> 1);
				}
			}
		}
		else if (rule == "factor AMP lvalue") {
			val lex = getLexLvalue(children(1));
			if (varsUsed(funcName) contains lex) {
				val fullName = funcName + " " + lex;
				if (unAddressedUsedVars contains fullName) {unAddressedUsedVars(fullName) = -10000;}
			}
			//remove from unAddressedUsedVars
		}
		else {
			for (c<- children) populateVarCounts(c, funcName);
		}
	}
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

	def generateCodeForStatements(stmts: Node, nWhile: Int, name: String, opt: Boolean) : Int = {
		val children = stmts.children;
		if (children.length == 2) {
			var newNumberOfWhiles = generateCodeForStatements(children(0), nWhile, name, opt);
			var newNumberOfWhiles2 = generateCodeForONEStatement(children(1), newNumberOfWhiles, name, opt);
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
			if(varsUsed(name) contains lexOfDCL) {
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
				var inst ="";
				if (offset == "0") inst+= "sw $3, "
				else inst+= "sw $3, -"
				inst+= offset.toString;
				inst+="($29)"
				MIPSOutput.append(inst);
			}
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
		//println(";lvalue.rule " + lvalue.rule)
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
			return " :( SURELY THEY DON:SS:ST HAVE THIS "

		}
	}

	def generateCodeForONEStatement(stmt: Node, nWhile: Int, name: String, opt: Boolean) : Int = {
		val children = stmt.children;
		if (stmt.rule.contains("PRINTLN")) {

			GenCodeForExpr.generate(children(2), name);
			Utils.push(1);


			val copyTo1 = "add $1, $3, $0";
			MIPSOutput.append(copyTo1);
			val lis10 = "lis $10";
			val printWord = ".word print"
			val callPrint = "jalr $10"
			MIPSOutput.append(lis10);
			MIPSOutput.append(printWord);
			MIPSOutput.append(callPrint);

			Utils.pop(1);
			
			return nWhile;
		}
		else if (stmt.rule == "statement lvalue BECOMES expr SEMI") {
			
			val expr = children(2);
			val lvalue = children(0);
			

			var inst ="";
			//println("; lvalue rule" + lvalue.rule);
			if (!lvalue.rule.contains("STAR")) {
				val lex = getLexLvalue(children(0));
				//check if it's mapped to a register.
				if (  !opt || (varsUsed(name) contains lex) || (doesExpHaveFuncCall(expr))) {
					//println("; generating code for "  + lex)
					GenCodeForExpr.generate(expr, name);
					
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
					println("; not generating code for " + lex)
				}
			}
			else {
				println("; normal code gen has star ")
				GenCodeForExpr.generate(expr, name);
				MIPSOutput.append("; de-ref a pointer and assign to it")
				Utils.push(3);
				generateCodeForLvalue(children(0), name);

				Utils.pop(5);

				val storeDereferencedVal = "sw $5, 0($3)"
				MIPSOutput.append(storeDereferencedVal);

			}
			return nWhile;
		}
		else if (stmt.rule.contains("WHILE")) {
			var newNumberOfWhiles = generateCodeForWhile(stmt, nWhile, name, opt);
			return newNumberOfWhiles;
		}
		else if (stmt.rule.contains("IF")) {
			var newNumberOfLabels = generateCodeForIF(stmt, nWhile, name, opt);
			return newNumberOfLabels;
		}
		else if (stmt.rule.contains("DELETE")) {
			MIPSOutput.append("; generating statement -> DELETE");
			GenCodeForExpr.generate(children(3), name);
			MIPSOutput.append("beq $3, $11, skipDeleteBitch" + nWhile)
			
			Utils.push(1);
			val set1 = "add $1, $0, $3";

			MIPSOutput.append(set1);

			val lis10 = "lis $10";
			val newWord = ".word delete"
			val call = "jalr $10"
			MIPSOutput.append(lis10);
			MIPSOutput.append(newWord);
			MIPSOutput.append(call);
			Utils.pop(1);

			MIPSOutput.append("skipDeleteBitch" + nWhile+ ":")
			return nWhile + 1;
		}
		else return nWhile;
	}

	def generateCodeForLvalue(lvalue: Node, funcName: String) : Unit = {

		GenCodeForLvalue.generate(lvalue, funcName);
	}

	def generateCodeForIF(stmt: Node, nLabels: Int, funcName: String, opt: Boolean) : Int = {
		MIPSOutput.append("; generating for if")
		val children = stmt.children;
		val test = children(2);
		val exp1 = test.children(0);
		val exp2 = test.children(2);
		var isAlwaysTrue = false;
		var isAlwaysFalse = false;
		println(";if isConstantExpr results: exp1 " + isConstantExpr(exp1, funcName).toString);
		println(";if isConstantExpr results: exp2 " + isConstantExpr(exp2, funcName).toString);
		if (isConstantExpr(exp1, funcName) && isConstantExpr(exp2, funcName)) {
			println("; is const if")
			val exp1Val = getNumExpr(exp1, funcName).toInt;
			val exp2Val = getNumExpr(exp2, funcName).toInt;
			println(";exp1Val " + exp1Val.toString + "  exp2Val " + exp2Val.toString)
			//println("; test.children(2).lex " + test.children(1).value)
			if (test.children(1).value == "EQ") {
				if (exp1Val != exp2Val) isAlwaysFalse = true;
				else isAlwaysTrue = true;
			}
			else if (test.children(1).value == "NE") {
				if (exp1Val == exp2Val) isAlwaysFalse = true;
				else isAlwaysTrue = true;
			}
			else if (test.children(1).value == "LT") {
				if (exp1Val >= exp2Val) isAlwaysFalse = true;
				else isAlwaysTrue = true;
			}
			else if (test.children(1).value == "LE") {
				if (exp1Val > exp2Val) isAlwaysFalse = true;
				else isAlwaysTrue = true;
			}
			else if (test.children(1).value == "GE") {
				if (exp1Val < exp2Val) isAlwaysFalse = true;
				else isAlwaysTrue = true;
			}
			else if (test.children(1).value == "GT") {
				println(";happens ")
				if (exp1Val <= exp2Val) isAlwaysFalse = true;
				else isAlwaysTrue = true;
			}
			println(";isAlwaysTrue " + isAlwaysTrue.toString + " isAlwaysFalse " + isAlwaysFalse.toString)
			if (isAlwaysTrue) { //generate for if clause
				println("; gen code for if ")
				generateCodeForStatements(children(5), nLabels, funcName, opt);
			}
			else if (isAlwaysFalse) {
				println("; gen code for else")
				generateCodeForStatements(children(9), nLabels, funcName, opt);
			}
			return nLabels;
		}
		else {

			generateCodeForTest(children(2), funcName);
			val startLabel = "sIF" + nLabels;
			val endLabel = "eIF" + nLabels;
			val branchToTrue = "bne $3, $0, " + startLabel;
			MIPSOutput.append(branchToTrue)
			var newNumberOfLabels = generateCodeForStatements(children(9), nLabels + 1, funcName, opt);
			val branchToEND = "beq $0, $0, " + endLabel;
			MIPSOutput.append(branchToEND);
			MIPSOutput.append(startLabel+":")
			newNumberOfLabels+= generateCodeForStatements(children(5), newNumberOfLabels + 1, funcName, opt);
			MIPSOutput.append(endLabel+":")
			return newNumberOfLabels;
		}
	}

	def generateCodeForWhile(stmt: Node, nWhile: Int, funcName: String, opt: Boolean) : Int = {
		val children = stmt.children;
		val test = children(2);
		val exp1 = test.children(0);
		val exp2 = test.children(2);
		var isDead = false;
		println("; " + isConstantExpr(exp1, funcName).toString +"  " + isConstantExpr(exp2, funcName).toString)
		if (isConstantExpr(exp1, funcName) && isConstantExpr(exp2, funcName)) {
			println("; is const")
			val exp1Val = getNumExpr(exp1, funcName).toInt;
			val exp2Val = getNumExpr(exp2, funcName).toInt;
			println(";exp1Val " + exp1Val.toString + "  exp2Val " + exp2Val.toString)
			println("; test.children(2).lex " + test.children(1).value)
			if (test.children(1).value == "EQ") {
				if (exp1Val != exp2Val) isDead = true;
			}
			else if (test.children(1).value == "NE") {
				if (exp1Val == exp2Val) isDead = true;
			}
			else if (test.children(1).value == "LT") {
				if (exp1Val >= exp2Val) isDead = true;
			}
			else if (test.children(1).value == "LE") {
				if (exp1Val > exp2Val) isDead = true;
			}
			else if (test.children(1).value == "GE") {
				if (exp1Val < exp2Val) isDead = true;
			}
			else if (test.children(1).value == "GT") {
				println(";happens ")
				if (exp1Val <= exp2Val) isDead = true;
			}
		}
		if (isDead) println("; detected dead code")
		if (!isDead) {
			MIPSOutput.append("; generating code for while ")
			
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
			var newNumberOfWhiles = generateCodeForStatements(children(5), nWhile + 1, funcName, opt);
			val goToStart1 = "lis $6";// + nWhile.toString;
			val goToStart2 = ".word sw" + nWhile.toString;
			val goToStart3 = "jr $6"
			MIPSOutput.append(goToStart1);
			MIPSOutput.append(goToStart2);
			MIPSOutput.append(goToStart3);
			MIPSOutput.append("ew" + nWhile.toString + ":");
			return newNumberOfWhiles ;
		}
		return nWhile;
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
			Utils.push(2)

			val make2Zero = "add $2, $0, $0";
			MIPSOutput.append(make2Zero)
		}

		val callInit = "jalr $10"
		MIPSOutput.append(callInit);
		if (firstParamType == "int") {
			Utils.pop(2);
		}

		val expr = currProcedure.children(11);
		val stmts = currProcedure.children(9);
		val dcls = currProcedure.children(8);

		generateCodeForDCLs(dcls, "wain");
		numberOfWhiles = numberOfWhiles + generateCodeForStatements(stmts, numberOfWhiles, "wain", true);
		GenCodeForExpr.generate(expr, "wain");
		
		MIPSOutput.addEpilog(size*4, "wain");
	}

	def generateCodeForProcedure(procedure: Node) : Unit = {
		val children = procedure.children;
		val name = children(1).lex;
		if (functionsUsed(name)) { //function is used somewhere
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
			numberOfWhiles = numberOfWhiles + generateCodeForStatements(stmts, numberOfWhiles, name, true);
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

	def populateUsedVarsInExpr(expr: Node, funcName: String) {
		val children = expr.children;
		//println("; rule " + expr.rule)
		if (expr.rule == "factor ID") {
			var varlex = children(0).lex;
		//	println("; varlex " + varlex)
			varsUsed(funcName) += varlex;
		}
		else {
			for (c<- children) {
				populateUsedVarsInExpr(c, funcName);
			}
		}
	}

	def doesExpHaveFuncCall(expr: Node) : Boolean = {
		if (expr.rule.contains("factor ID LPAREN")) return true;
		else if (expr.children.length > 0) {
			for (c<- expr.children) {
				val funcCall = doesExpHaveFuncCall(c)
				if (funcCall) return true;
			}
			return false;
		}
		else return false;
	}

	def doesLvalueHaveStar(lval: Node) : Boolean = {
		if (lval.rule == "lvalue ID") return false;
		else if (lval.rule == "lvalue LPAREN lvalue RPAREN") return doesLvalueHaveStar(lval.children(1));
		else if (lval.rule == "lvalue STAR factor") return true;
		else return false;
	}

	def populateUsedVarsInStmts(stmts: Node, funcName: String) {
		//println("populateUsedVarsInStmts "+ stmts.rule)
		val children = stmts.children;
		if (children.length == 2) {
			val stmt = children(1);
			val others = children(0);
			
			if (stmt.rule.contains("PRINTLN")) {
				//println("called with ")
				val expr = stmt.children(2);
				populateUsedVarsInExpr(expr, funcName);
			}
			else if (stmt.rule.contains("DELETE")) {
				val expr = stmt.children(3)
				populateUsedVarsInExpr(expr, funcName)
			}
			else if (stmt.rule.contains("IF")) {
				val expr = stmt.children(2);
				populateUsedVarsInExpr(expr, funcName);
				val ifstmts = stmt.children(5);
				val elsestmts = stmt.children(9);

				populateUsedVarsInStmts(elsestmts, funcName);
				populateUsedVarsInStmts(ifstmts, funcName);

				
			}
			else if (stmt.rule.contains("WHILE")) {
				val expr = stmt.children(2);
				populateUsedVarsInExpr(expr, funcName);
				val inloopstmt = stmt.children(5);
				populateUsedVarsInStmts(inloopstmt, funcName);
			}
			else if (stmt.rule.contains("BECOMES")) {
				val lex = getLexLvalue(stmt.children(0));
				val expr = stmt.children(2);
				if ((varsUsed(funcName) contains lex) ||doesLvalueHaveStar(stmt.children(0))) {

					populateUsedVarsInExpr(expr, funcName);
				}

			}
			populateUsedVarsInStmts(others, funcName)
		}
	}

	def findUsedVars(procedures: Node) {
		val children = procedures.children;
		val empty = Set[String]();
		if (procedures.rule.contains("main")) {
			val MainChildren = children(0).children;
			val expr = MainChildren(11);
			val stmts = MainChildren(9);
			varsUsed = varsUsed + ("wain" -> empty)
			populateUsedVarsInExpr(expr, "wain");
			populateUsedVarsInStmts(stmts, "wain");
			
		}
		else if (procedures.rule == "procedures procedure procedures") {
			val proc = children(0);
			val procstmts = proc.children(7);
			val procexpr = proc.children(9);
			val id = proc.children(1).lex;
			varsUsed = varsUsed + (id -> empty)
			populateUsedVarsInExpr(procexpr, id);
			populateUsedVarsInStmts(procstmts, id);
			findUsedVars(children(1));
		}
	}
	def populateConsts(dcls: Node, funcName: String) {
		//println("; called with " + funcName)
		//println("; rule here " + dcls.rule)
		if (dcls.rule == "dcls dcls dcl BECOMES NUM SEMI") {
			val lex = dcls.children(1).children(1).lex;
			val num = dcls.children(3).lex;
			val fullName = funcName + " " + lex;
			//println("dcls full Name " + fullName)
			constMapping = constMapping + (fullName -> num);
			populateConsts(dcls.children(0), funcName);
		}
		else if (dcls.rule == "dcls dcls dcl BECOMES NULL SEMI") {
			populateConsts(dcls.children(0), funcName);
		}
	}
	def removeFromConsts(stmts: Node, funcName: String) {
		val rule = stmts.rule;
		val children = stmts.children;
		if (rule == "statements statements statement") {
			val s = children(1);
			//println("; one statement rule " + s.rule)
			if (s.rule == "statement lvalue BECOMES expr SEMI") {
				val lval_lex = getLexLvalue(s.children(0));
				val fullName = funcName + " "  + lval_lex;
			//	println("; removing from constMapping " + fullName);
				val expr = s.children(2);	
				constMapping = constMapping - fullName;
				
			}
			else if (s.rule.contains("WHILE")) removeFromConsts(s.children(5), funcName);
			else if (s.rule.contains("IF")) {
				removeFromConsts(s.children(5), funcName);
				removeFromConsts(s.children(9), funcName);
			}
			removeFromConsts(children(0), funcName);
		}
	}
	def identifyConsts(procedures: Node) {
		val children = procedures.children;
		if (procedures.rule.contains("main")) {
			val MainChildren = children(0).children;
			val expr = MainChildren(11);
			val stmts = MainChildren(9);
			val dcls = MainChildren(8);
			populateConsts(dcls, "wain");
			//varsUsed = varsUsed + ("wain" -> empty)
			//populateUsedVarsInExpr(expr, "wain");
			removeFromConsts(stmts, "wain");
			
		}
		else if (procedures.rule == "procedures procedure procedures") {
			val proc = children(0);
			val procstmts = proc.children(7);
			val procexpr = proc.children(9);
			val id = proc.children(1).lex;
			val dcls = proc.children(6);
			populateConsts(dcls, id);
			//println("; just populated for function " + id)
			
			//varsUsed = varsUsed + (id -> empty)
			//populateUsedVarsInExpr(procexpr, id);
			removeFromConsts(procstmts, id);
			identifyConsts(children(1));
		}
	}

	

	def getMostUsedVars(howManyMostUsed: Int) {
		val sorted = unAddressedUsedVars.toSeq.sortBy(x => x._2.toInt).reverse;
		println("; ordered by usage ");
		var rCount = 0;
		for (c<- sorted) {
			if (rCount < howManyMostUsed) { 
				if (c._2 > 0) {
					MappingToRegisters = MappingToRegisters + (c._1 -> availableRegisters(rCount));
					rCount = rCount + 1;
				}
			}
			//println("; var " + c._1 +  " count " + c._2);
		}
	}

	def main(args: Array[String]) : Unit = {
		
		ParseTree = ParseTreeBuilder.construct(ParseTree).children(0);

		var symTable = ArrayBuffer[FunctionSymTable]();
		symTable = SymbolTableBuilder.buildSymbolTable(ParseTree, symTable, "");
		
		FINALSYMTABLE = symTable;
		//populateFunctionsUsedInWain(ParseTree);
		val procedures = ParseTree.children(1);

		populateFunctionsUsedProcedures(procedures);

		findUsedVars(procedures);

		identifyConsts(procedures);
		getMostUsedVars(10); //10 most used non constant variables
		var actuallyUsed = MappingToRegisters.size;
		var setOfAvailRegs = availableRegisters.slice(actuallyUsed, availableRegisters.length).toSet;

		for ((k, v)<- MappingToRegisters) {
			println(";function var " + k);
			println("; mapped to " + v);
		}
		for (r<- setOfAvailRegs) {
			println(";available: " + r)
		}

		GenCodeForFactor.init(MappingToRegisters);
		signatureMap = SymbolTableBuilder.getSignatureMap();
		TypeChecker.setup(signatureMap, signatureMap, FINALSYMTABLE)
		MIPSOutput.init();
		Utils.init(FINALSYMTABLE, constMapping);
		GenCodeForExpr.init(FINALSYMTABLE);
		GenCodeForTest.init(FINALSYMTABLE);

		//SymbolTableBuilder.debugPrintSymTable(FINALSYMTABLE)
		//generateCodeForStatements()
		generateCode(procedures);
		//SymbolTableBuilder.debugPrintSymTable(symTable);
	}


}