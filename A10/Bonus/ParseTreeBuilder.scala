import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map;

import gen.Node;

object ParseTreeBuilder {
	var in = Source.fromInputStream(System.in).getLines;
	def isTerminal(s: String): Boolean =  {
		return s == s.toUpperCase
	}
	def printTree(tree: Node) : Unit = {
		println("node val " + tree.value);
		println("node derivation " + tree.rule)
		tree.children.map(x=>print(x.value + " <3 "))
		println("")
		for (c<- tree.children) {
			printTree(c)
		}
	}
	def buildParseTree(tree: Node, derivation: String) : Node = { //ADD A BASE CASE for derivation being null
		//ADD A BASE CASE
		if (derivation == "") return tree;
		var derivationArr = derivation.split(" ");
		val LHS = derivationArr(0);
		var RHS = derivationArr.drop(1);
		//if (LHS == "start") println("RHS len " + RHS.length )
		if (isTerminal(LHS)) {

			tree.children += new Node(LHS, derivation);


			tree.children(tree.children.length - 1).lex = RHS(0);
		}
		else {
			tree.children += new Node(LHS, derivation)
			for (r <- RHS) {
				//if (LHS == "start") println("rhs r " + r);
				var next = in.next;

				buildParseTree(tree.children(tree.children.length - 1), next)

			}
		}
		return tree;
	}
	def construct(tree: Node) : Node = {
		return buildParseTree(tree, in.next);
	}
}