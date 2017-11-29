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
		var actualSize = sizeSymTable ;
		//if (name != "wain") actualSize = actualSize - 4;
		//println(";adding prolog for " + name);
		output+= "; adding prolog for " + name;
		if(name == "wain") {
			output+= "sub $29, $30, $4"
			output+= "add $7, $31, $0"
		}
		

		output+= "lis $6"
		output+= s".word $actualSize"
		output+= "sub $30, $30, $6"
		
		if (name == "wain") { 
			output+= "sw $1, 0($29)"
			output+= "sw $2, -4($29)"
		//	output+= "sw $31, -" + sizeSymTable.toString+ "($29)"
		}
		Utils.push(31);
		
		output+= "; prolog ends here for " + name;

	}
	def addEpilog(sizeSymTable: Int, name: String) {
		//output+="; epilog begins here for " + name;
		Utils.pop(31);
		output+="add $30, $29, $4"
		if (name == "wain") {
			output+="add $31, $7, $0"
		//	output+="lw $31, -"+ sizeSymTable.toString+ "($29)";
			
		}
		output+="jr $31"
	}
	def printOutput() {
		for (inst <- output) println(inst);
	}
	def main(args: Array[String]) : Unit = {}
}