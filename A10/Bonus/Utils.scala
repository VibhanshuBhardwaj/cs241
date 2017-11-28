import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map; //[String, Int]()

import gen.Node;
import MIPSOutput._;

object Utils {
	var FINALSYMTABLE = ArrayBuffer[FunctionSymTable]();

	type FunctionSymTable = (String, Map[String, String], Int);


	def getValOfLexFromSymTable(lex: String, funcName: String) : String = {
		for (f <- FINALSYMTABLE) {
			if (f._1 == funcName) {
				return f._2(lex);
			}
		}
		println(";SHOULD NOT BE HAPPENING YET");
		return "";
	}
	def push(r: Int) {
		var reg = "$" + r.toString;
		var inst = "sw " + reg + ", " + "-4($30)";
		MIPSOutput.append(inst);
		MIPSOutput.append("sub $30, $30, $4");
	}
	def pop(r: Int) {
		var reg = "$" + r.toString;
		var inst = "lw " + reg + ", " + "-4($30)";
		MIPSOutput.append("add $30, $30, $4");
		MIPSOutput.append(inst);
	}
	def init(table: ArrayBuffer[FunctionSymTable]) {
		FINALSYMTABLE =table;
	}
}