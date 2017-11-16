import scala.collection.mutable.ArrayBuffer
import scala.io.Source;
import scala.collection.mutable.Stack
object LR {
	var states = ArrayBuffer[String]();
	var symbols = ArrayBuffer[String]();
	var derivations = ArrayBuffer[String]();
	var rules = ArrayBuffer[String]();
	var START = "";
	var resultDerivations = ArrayBuffer[String]();

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
	def reduce(rule: String) {
		//println("inside reduce with " + rule)
		resultDerivations += rule;
		var rArr = rule.split(" ");
		for (i<-0 to rArr.length - 2) states.remove(states.size -1);
		for (i<-0 to rArr.length - 2) symbols.remove(states.size -1);
		//rArr.map(x => states.remove(states.size -1));
		//rArr.map(x => symbols.remove(states.size -1));
		//println("shift called from reduce with " + rArr(0) + " " + getDerivation(rArr(0)).replaceAll(".*shift ", ""))
		//println("after removes");
		//rArr.map(x => print(x) + "");
		//println("");
		shift(rArr(0), getDerivation(rArr(0)).replaceAll(".*shift ", ""));
	}
	def shift(sym: String, state: String) : Unit = {
		states += state;
		symbols += sym;
	}
	def getDerivation(currToken: String) : String = {
		for (d <- derivations) {
			var correctStart = states(states.size  - 1 );
			correctStart+= " " + currToken;
			// /println("correctStart " + correctStart)
			if (d.startsWith(correctStart)) return d;
		}
		return "INVALID";
	}
	def main(args: Array[String]) : Unit = {
		var input = Source.fromInputStream(System.in).getLines;
		input = extractInfoFromInput(input);
		//rules.map(println);
		//println("map over")
		//derivations.map(println)
		var toDerive = "";
		while(input.hasNext) {
			toDerive = toDerive +  input.next.toString;
			toDerive+= " "; //jsut have whitespacce instead of newline
		}

		var toDeriveArr = toDerive.split(" "); //FLAG
		states += "0";
		//println("toDeriveArr ")
		//toDeriveArr.map(x => print(x + " "))
		var line_number = 0;

		for (i <- 0 to toDeriveArr.length - 1) {
			var currToken = toDeriveArr(i)
			var derivation = getDerivation(currToken);
			//println("currToken " + currToken + " derivation " + derivation)
			if (derivation == "INVALID") {
				if (line_number > 50000) line_number = line_number + 1;
				sys.error("ERROR at " + line_number);
				System.exit(1);
			}

			if (derivation contains "shift" ) {

				var sanitizedDerivation = derivation.replaceAll(".*shift ","");
				shift(currToken, sanitizedDerivation);

			}

			else if (derivation contains "reduce" ) {
				//println("reduce called withh " + rules(derivation.replaceAll(".*reduce ","").toInt));
				reduce(rules(derivation.replaceAll(".*reduce ","").toInt));
				var breakBool = true;
				while(breakBool) {
					var nextDerivation = getDerivation(currToken);
					if(nextDerivation contains "reduce" ) {
						reduce(rules((nextDerivation.replaceAll(".*reduce ","").toInt)));
					}
					else {
						shift(currToken, nextDerivation.replaceAll(".*shift ",""));
						breakBool = false;
					}
				}
			}
			line_number = line_number + 1;
		}
		for(i<-0 to rules.length - 1) {
			if(rules(i).startsWith(START + " "))
				reduce(rules(i));
		}
		for (i <- 0 to resultDerivations.length -1) {
			println(resultDerivations(i))
		}
	}
}