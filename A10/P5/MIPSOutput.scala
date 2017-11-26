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
	def init() {
		output+= ".import print"
		output+= ".import init"
		output+= ".import delete"
		output+= ".import new"
		output+= "lis $4"
		output+= ".word 4"
		output+= "lis $11"
		output+= ".word 1"
		output+= "beq $0, $0, Fwain"
	}
	def addProlog(sizeSymTable: Int, name: String) {
		val actualSize = 4 + sizeSymTable ;
		println(";adding prolog for " + name);
		output+= "; adding prolog for " + name;
		output+= "sub $29, $30, $4"
		output+= "lis $12"
		output+= s".word $actualSize"
		output+= "sub $30, $30, $12"
		if (name == "wain") { 
			output+= "sw $1, 0($29)"
			output+= "sw $2, -4($29)"
		}
		output+= "sw $31, -" + sizeSymTable.toString+ "($29)"
		output+= "; prolog ends here for " + name;

	}
	def addEpilog(sizeSymTable: Int, name: String) {
		output+="; epilog begins here for " + name;
		output+="lw $31, -"+ sizeSymTable.toString+ "($29)";
		output+="add $30, $29, $4"
		output+="jr $31"
	}
	def printOutput() {
		for (inst <- output) println(inst);
	}
	def main(args: Array[String]) : Unit = {}
}