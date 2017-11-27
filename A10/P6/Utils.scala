import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map; //[String, Int]()

import gen.Node;

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
	def init(table: ArrayBuffer[FunctionSymTable]) {
		FINALSYMTABLE =table;
	}
}