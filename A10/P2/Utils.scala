import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map; //[String, Int]()

import gen.Node;

object Utils {
	var FINALSYMTABLE = ArrayBuffer[FunctionSymTable]();

	type FunctionSymTable = (String, Map[String, String], Int);


	def getValOfLexFromSymTable(lex: String) : String = {
		for (f <- FINALSYMTABLE) {
			if (f._1 == "wain") {
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