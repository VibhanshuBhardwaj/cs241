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

	def printOutput() {
		for (inst <- output) println(inst);
	}
	def main(args: Array[String]) : Unit = {}
}