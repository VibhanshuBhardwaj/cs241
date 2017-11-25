import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map;

object MIPSOutput {
	var output =  ArrayBuffer[String]();
	def append(s: String) : ArrayBuffer[String] = {
		output += s;
		return output;
	}
	def addProlog(sizeSymTable: Int) {
		output+= "lis $4"
		output+= ".word 4"
		output+= "sub $29, $30, $4"
		output+= "lis $12"
		output+= s".word $sizeSymTable"
		output+= "sub $30, $30, $12"
		output+= "sw $1, 0($29)"
		output+= "sw $2, -4($29)"
		output+= "; prolog ends here"

	}
	def addEpilog() {
		output+="; epilog begins here"
		output+="add $30, $29, $4"
		output+="jr $31"
	}
	def printOutput() {
		for (inst <- output) println(inst);
	}
	def main(args: Array[String]) : Unit = {}
}