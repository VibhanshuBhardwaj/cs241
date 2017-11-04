import scala.io.Source;
import scala.collection.mutable.Stack

object Galaxy {

	def skipGrammar(in: Iterator[String])= {
	    assert(in.hasNext)

		// read the number of terminals and move to the next line
		val numTerm = in.next.replace('\n',' ').trim.toInt

		// skip the lines containing the terminals
		for (i <- 1 to numTerm) in.next
		
		// read the number of non-terminals and move to the next line
		val numNonTerm = in.next.replace('\n',' ').trim.toInt

		// skip the lines containing the non-terminals
		for (i <- 1 to numNonTerm) in.next

		// skip the line containing the start symbol
		in.next

		// read the number of rules and move to the next line
		val numRules = in.next.replace('\n',' ').trim.toInt

		// skip the lines containing the production rules
		for (i <- 1 to numRules) in.next
		in.next; //skip the start as well
	}
	def getFinalExp(in: Iterator[String]) : String = {
		val start = "S BOF expr EOF";
		var currExp = start;
		while (in.hasNext) {
			var line = in.next;
			//System.out.println("processing " + line)
			var i = 0
			while (line.charAt(i) == ' ') { //get the last index of leading whitespaces
				i = i + 1;
			}
			line = line.substring(i); //strip leading whitespaces away 
			//lhs and rhs of derivation
			val arr = line.split(" ", 2);
			//System.out.println("arr ");
			//arr.map(print)
			//println("")
			val lhs = line.split(" ", 2)(0);
			val rhs = line.split(" ", 2)(1);
			//System.out.println("lhs: " + lhs);
			//System.out.println("rhs: " + rhs);
			//System.out.println("before replacing " + currExp);
			currExp = currExp.replaceFirst(lhs, rhs)
			//System.out.println("after replacing " + currExp);

		}
		val len = currExp.length();
		return currExp.substring(6, len - 4);
	}
	def evaluate(exp: String, currResult: Int, isAdd: Boolean) : Int = {
		if (exp.length() == 0) return currResult;
		else if (exp.charAt(0) == 'i') {
			var newResult = 0;
			if (isAdd) newResult = currResult + 42;
			else newResult = currResult - 42;
			return evaluate(exp.substring(2), newResult, true)
		}
		else if (exp.charAt(0) == '-') return evaluate(exp.substring(1), currResult, false);
		else { //we see a paranthesis
			var index = 0;
			val len = exp.length() - 1;
			for (i <- 0 to len) {
				if (exp.charAt(i) == ')') {index = index  - 1;}
				else if (exp.charAt(i) =='(') {index = index + 1;}
				if (index == 0) {
					index = i;
					//break;
					var newResult = 0;
					if (isAdd) newResult = currResult + 42;
					else newResult = currResult - 42;
					return evaluate(exp.substring(index + 1), newResult, true)
				}
			}
			return currResult;
			
		}
	}
	def eval2(exp: String) : Int = {
		var vals = Stack[Int]();
		var ops = Stack[String]();
		val splitExp = exp.split(" ");
		//splitExp.map(x => print(x + " "))
		for (c <- splitExp) {
			//println("c " + c);
			if ( c == "42" ) {vals.push(c.toInt)}
			else if (c == "(") {

			}
			else if (c == "-") {
				//ops.push(c);
			}
			else if (c == ")") {
				//ops.pop();
				var value = vals.pop();
				if (!vals.isEmpty) {value = vals.pop() - value;}
				//println("new val pushed " + value)
				vals.push(value);
			}
			

		}
		return vals.pop();
	}
	def main(args: Array[String]) : Unit = {
		val input = Source.fromInputStream(System.in).getLines;
		skipGrammar(input);
		var finalExp = getFinalExp(input);
		System.out.println("finalExp" + finalExp)
		finalExp = "( "+finalExp.replaceAll("id", "42") +" )";
		System.out.println("finalExp" + finalExp)
		System.out.println(eval2(finalExp));


		//now input just has the derivation
		
		

	}
}