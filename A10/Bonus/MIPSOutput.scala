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
	val availableRegisters = Array("9", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27");
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
		for (r<- availableRegisters) {
			Utils.push(r.toInt)
		}
	}
	def addProlog(sizeSymTable: Int, name: String) {
		var actualSize = sizeSymTable ;
		//if (name != "wain") actualSize = actualSize - 4;
		//println(";adding prolog for " + name);
		output+= "; adding prolog for " + name;
		if(name == "wain") {
			output+= "sub $29, $30, $4"
			output+= "add $28, $31, $0"
		}
		

		output+= "lis $12"
		output+= s".word $actualSize"
		output+= "sub $30, $30, $12"
		
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
			output+="add $31, $28, $0"
			for (r<- availableRegisters.reverse) {
				Utils.pop(r.toInt)
			}
		//	output+="lw $31, -"+ sizeSymTable.toString+ "($29)";
			
		}
		output+="jr $31"
	}
	def printOutput(comments: Boolean) {
		for (inst <- output) {
			if (comments) println(inst);
			else {
				if (!inst.startsWith(";")) println(inst);
			}
		}
	}
	def main(args: Array[String]) : Unit = {}
}