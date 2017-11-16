import scala.collection.mutable.ArrayBuffer
import scala.io.Source;
import scala.collection.mutable.Stack
object LR {
	var states = ArrayBuffer[String]();
	var symbols = ArrayBuffer[String]();
	var derivations = ArrayBuffer[String]();
	var rules = ArrayBuffer[String]();
	var START = "";

	def extractInfoFromInput(in: Iterator[String]) : Iterator[String] = {
		var countOfX = in.next.toInt;
		for ( i<- 1 to countOfX) in.next;
		countOfX = in.next.toInt;
		for ( i<- 1 to countOfX) in.next;
		START = in.next;
		countOfX = in.next.toInt;
		for ( i<- 1 to countOfX) rules += in.next;
		in.next;
		countOfX = in.next.toInt;
		for ( i<- 1 to countOfX) derivations += in.next;
		return in;


	}
	def main(args: Array[String]) : Unit = {
		var input = Source.fromInputStream(System.in).getLines;
		input = extractInfoFromInput(input);
		//rules.map(println);
		//println("map over")
		//derivations.map(println);
		while(input.hasNext) {
			val n_x_line = input.next;
			var foundRule = false;
			for (i<-0 to derivations.length - 1) {
				if ((derivations(i) contains "reduce") && (derivations(i).startsWith(n_x_line))) {
					//println("ith derivation:" + derivations(i) + "indexOf reduce " +derivations(i).indexOf("reduce "))
					var ruleIndex = derivations(i).substring(derivations(i).indexOf("reduce ")+7).toInt;
					println("reduce " + rules(ruleIndex));
					foundRule = true;
				}
				else if ((derivations(i) contains "shift") && (derivations(i).startsWith(n_x_line)) ){
					//println("index for shift " + derivations(i).indexOf(n_x_line) + n_x_line.length);
					//println("ith derivation" + derivations(i))
					val shiftRule = derivations(i).substring(derivations(i).indexOf(n_x_line) + n_x_line.length+1);
					if (shiftRule.length > 1) {
						println(shiftRule);
						foundRule = true;
					}
					else {
						println("error");
					}
				}

			}
			if (!foundRule) {
				println("error");
			}
		}
		

	}
}