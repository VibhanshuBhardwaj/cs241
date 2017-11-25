import scala.collection.mutable.ArrayBuffer
import scala.io.Source;
import scala.collection.mutable.Stack
import parser.Transition
import parser.Tree
import parser.Token

object WLP4Parse {

	var input = ArrayBuffer[Token]();
	var output = ArrayBuffer[String]();

	var terminals = Set[String]();
	var nonTerminals = Set[String]();
	
	var rules = ArrayBuffer[String]();
	var START = "S";
	var transitions = ArrayBuffer[Transition]();
	var symbols = Stack[Token]();
	var states = Stack[String]();

	var resultDerivations = ArrayBuffer[String]();
	def getLR1Info() {
		val in = scala.io.Source.fromFile("WLP4.lr1").getLines;
		var nTerms = in.next.toInt;
		for (i <- 1 to nTerms) {
			terminals += in.next;
		}
		nTerms = in.next.toInt;
		for (i <- 1 to nTerms) {
			nonTerminals += in.next;
		}
		START = in.next;
		nTerms = in.next.toInt;
		for (i <- 1 to nTerms) {
			rules += in.next;
		}
		in.next;
		nTerms = in.next.toInt;
		for (i <- 1 to nTerms) {
			var tString = in.next;
			//println("tString " + tString);
			var arr = tString.split(" ");
			//tString.map(println)
			//println("arr " + arr.length)
			// /println(" read transition " + tString);
			val t = new Transition(arr(0), arr(1), arr(2), arr(3));

			transitions += t;
		}
		//rules.map(println)

	}
	def traverse (t: Tree) {
		println(t.rule);
		val arr = t.rile.split(" ");
		for (i <- 1 to arr.length - 1) {
			if (terminals contains arr[i]) {
				println(arr(i) + " " + input(0).lexeme);
				input.remove(0);
			}
			else {
				traverse(t.children(0));
				t.children.remove(0);
			}
		}
	}

	def lr() {
		var tokenstr = "";
		var stack = new Stack[Tree]();
		var counter = 0;
		do {
			var arrList = new ArrayBuffer[String]();
			var outputAtI = output(counter);
			var tokens = outputAtI.split(" ");
			tokenstr = tokens(0);
			for (i <- 1 to tokens.length - 1) {
				if (nonTerminals contains tokens(i)) arrList += tokens(i);

			}
			pooper(arrList, outputAtI, stack);
			counter = counter + 1;
		} while (START != tokenstr);
		return stack.top;

	}
	def getTransition(next: Token) : Transition =  {
		for (t <- transitions) {
			if ((t.nextToken == next.kind) && (t.from == states.top)) return t;
		}
		return null;
	}
	def pooper(rhs: List[String], rule: String, stack: Stack[Tree]) {
		var t = new Tree(rule);
		for (s <- rhs) {
			val popped =stack.pop();
			t.children =  popped +: t.children;
		}
		stack.push(t);
	}

	def reduce (trans: Transition, i: Int) {
		var r = rules(trans.to.toInt);
		var splitR = r.split(" ");
		for (i<-0 to splitR.length - 2) {
			states.pop();
			symbols.pop();
		}
		val newToken = new Token(splitR(0), "dummy");
		var top = symbols.top;
		val nextTransition = getTransition(top);
		 //FLAG
		if (nextTransition !=null) {
			states.push(nextTransition.to);
			output += r;
		}
		else {
			System.err.println("ERROR: Syntax error at token " + i);
			System.exit(1);
		}

	}
	def shift(t: Token, s: String) {
		states.push(s);
		symbols.push(t);
	}
	def getTokensFromInput() {
		var in = Source.fromInputStream(System.in).getLines;
		input += new Token("BOF", "BOF");
		while(in.hasNext) {
			val kindLex = in.next.split(" ");

			input+= new Token(kindLex(0), kindLex(1));
		}
		input+= new Token("EOF", "EOF");
	}
	def main(args: Array[String]) : Unit = {
		getLR1Info();
		getTokensFromInput();
		states.push("0");
		for (i<- 0 to input.length -1) {
			var bool = true;
			while (bool) {
				var t = getTransition(input(i));
				if (!t) {
					System.err.println("ERROR: Syntax erro at token " + i);
					System.exit(1);
				}
				else if ("REDUCE" == t.tType) {
					reduce(t, i);
				}
				else bool = false;
			}
			shift(input(i), t.to);

		}
		output += rules(states(0))

	}

}
