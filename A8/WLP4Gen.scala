import scala.collection.mutable.ArrayBuffer;
import scala.io.Source;
import scala.collection.mutable.Stack;

import gen.Node;

object WLP4Gen {
	var input = ArrayBuffer[String]();
	var in = Source.fromInputStream(System.in).getLines;
	def readInput() : Unit = {
		var in = Source.fromInputStream(System.in).getLines;
		
		while (in.hasNext) {
			input += in.next;
		}
		
		//ParseTree.children += "start";
/*		var indexInInput = 0;
		while (true) {
			if (indexInInput > input.length - 1) {
				return ParseTree;
			}
			val derivation : Seq
		}

		input.map(println)*/
		//}
	}
	def isTerminal(s: String): Boolean =  {
		return s == s.toUpperCase
	}
	// def addToParseTree(tree: Node, indexInInput: Int, nChildren: Int): Node = {
	// 	println("indexInInput " + indexInInput + " nChildren " + nChildren);
	// 	if  ((indexInInput > input.length - 1)) return tree;
	// 	//println("index " + indexInInput)
	// 	println("adding " + input(indexInInput))
	// 	println("to tree.value " + tree.value);
	// 	val derivation = input(indexInInput).split(" ");
	// 	//println("read derivation ");
	// 	//derivation.map(x=> print(x + " "));
	// 	//println("")
	// 	//val derivationArr = derivation.split(" ").toSeq;
		
	// 	val LHS = derivation(0);
	// 	val RHS = derivation.drop(1);
	// 	tree.children += new Node(LHS);
	// 	if (isTerminal(LHS)) {
	// 		tree.children(tree.children.length - 1).lex = RHS(0);
	// 		val nextIndex = indexInInput + 1;
	// 		addToParseTree(tree, nextIndex, nChildren - 1);
	// 		return tree;//(tree)
	// 	}
	// 	else {
	// 		var nextIndex = indexInInput + 1;
	// 		var lastChild = tree.children(tree.children.length - 1);
	// 		//nextIndex = nextIndex + 1;
	// 		//println("nextIndex " + nextIndex)
	// 		//println("indexInInput + nChildren " + (indexInInput + nChildren))
	// 		if (nextIndex <= indexInInput + nChildren) {
	// 			println("happes");
	// 			//tree.children(tree.children.length - 1) = addToParseTree(lastChild, nextIndex, RHS.length);
	// 			addToParseTree(lastChild, nextIndex, RHS.length);
	// 			return tree.children(tree.children.length - 1);
	// 			//return tree;
	// 		}
	// 		else {
	// 			addToParseTree(tree, nextIndex, 1);
	// 			return tree;
	// 		}
	// 		// for (s <- RHS) {
	// 		// 	println("for loop called with " + s + " and index " + indexInInput)
	// 		// 	var lastChild = tree.children(tree.children.length - 1);
	// 		// 	nextIndex = nextIndex + 1;
	// 		// 	tree.children(tree.children.length - 1) = addToParseTree(lastChild, nextIndex);
	// 		// }
	// 		//return tree;
			
	// 	}
	// 	//addToParseTree(tree.children(tree.children.length - 1), RHS)

	// }
/*	def buildTree(tree: Node, index: Int) : Node = {
		println("index " + index)
		println("adding to tree rooted at " + tree.value)
		if (index > input.length - 1) return tree;
		var derivation = input(index);
		println(" derivation " + derivation)
		var derivationArr = derivation.split(" ");
		val LHS = derivationArr(0);
		var RHS = derivationArr.drop(1);
		tree.children += new Node(LHS);
		if (isTerminal(LHS)) {
			tree.children(tree.children.length - 1).lex = RHS(0);
			println("terminal " + LHS  + " added to " + tree.value)
		}
		else {
			println("non terminal " + LHS + " added to " + tree.value)
			val howManyRHS = RHS.length;
			var j = index;
			for (i<-1 to howManyRHS) {
				j = j + i ;
				val lastChild = tree.children(tree.children.length - 1);
				//println("adding the result of rec call to children of " + lastChild.value)
				//println("building with derivation " + input(j))
				tree.children(tree.children.length - 1) = buildTree(lastChild, j)
			}
		}
		//println("added " + derivationArr(0) +" as a child of " + tree.value)
		
		//for (i<- 0 to RHS.length - 1) {
	//		println("i is " + i)
	//		val lastChild = tree.children(tree.children.length - 1);
	//		tree.children(tree.children.length - 1).children += buildTree(lastChild, index + 1 +i)
	//	}
		return tree;

	}*/
	//returs tree to which next node is to be added
	def addToTree(tree: Node, index: Int) : Node = {
		if (index > input.length - 1) return tree;
		var derivation = input(index);
		println(" derivation " + derivation)
		var derivationArr = derivation.split(" ");
		val LHS = derivationArr(0);
		var RHS = derivationArr.drop(1);
		if (isTerminal(LHS)) {
			tree.children += new Node(LHS, RHS(0));
			return tree;
		}
		else {
			tree.children += new Node(LHS);
			var nindex = index;
			for (r <- RHS) {
				nindex = nindex + 1;
				addToTree(tree.children(tree.children.length - 1), nindex)
			}
			return tree;
		}
	}
	def printTree(tree: Node) : Unit = {
		println(tree.value);
		tree.children.map(x=>print(x.value + " <3 "))
		println("")
		for (c<- tree.children) {
			printTree(c)
		}
	}
	def lastTry(tree: Node, derivation: String) : Node = {
		// if (tree.value == "start") {
		// 	println("now we're doing something to stat")
		// }
		//println("adding " + derivation + " to " + tree.value)
		//println("tree val " + tree.value);
		//println("its children")
		//tree.children.map(x=>print(x.value + "--"))
		var derivationArr = derivation.split(" ");
		val LHS = derivationArr(0);
		var RHS = derivationArr.drop(1);
		//if (LHS == "start") println("RHS len " + RHS.length )
		if (isTerminal(LHS)) {

			tree.children += new Node(LHS);
		//	println("in if after adding terminal ");
			//tree.children.map(x=> print(x.value + ", "))

			tree.children(tree.children.length - 1).lex = RHS(0);
			//return tree;
		}
		else {
		//	println("adding " + LHS + " to this trees children " + tree.value)
			tree.children += new Node(LHS)
			for (r <- RHS) {
				if (LHS == "start") println("rhs r " + r);
				var next = in.next;

				//tree.children(tree.children.length - 1).children += lastTry(tree.children(tree.children.length - 1), next)
				lastTry(tree.children(tree.children.length - 1), next)
		//		println("after rec call children of " + tree.children(tree.children.length - 1).value)
				//tree.children(tree.children.length - 1).children.map(x=> print(x.value + " "))
			}
		}
		//println("returning " + tree.value)
		return tree;
	}
	def main(args: Array[String]) : Unit = {
		//var input
		//readInput();
		var ParseTree = new Node("ROOT");
		//println("input.length - 1 " + (input.length - 1) )
		ParseTree = lastTry(ParseTree, in.next);
		printTree(ParseTree)
		//println("root child len shud be 1 " + ParseTree.children.length)
		//var mainNode = ParseTree.children(0).children(2).children(0);
		// /println("mainNode children len " + mainNode.children.length);
		//mainNode.children.map(x=> print(x.value + "--"))
		//printTree(rootRemoved)
	}

}