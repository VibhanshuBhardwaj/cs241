import scala.io.StdIn

class Node {

	var Data: Int = -1;
	var Children: Seq[Node] = Seq[Nothing]();
	var NumberOfChildren: Int = 0;

	def this(value: Int, nChildren: Int) = {
		this();
		this.Data = value;
		this.NumberOfChildren = nChildren;
	}

	def AppendToChildren(child: Node) = {
		Children = Children :+ child;
	}

	def LengthOfChildren() : Int = {
		return Children.length;
	}
	def pprint() : Unit = {
		if (this == null) return;
		else {
			println(this.Data)
			var s = ""
			for (c <- this.Children) {
				s = s + c.Data.toString + " ";
			}
			println("children are " + s)
			for (c <- this.Children) {
				c.pprint();
			}
		}
	}
}


object AddNodeObj {

	//just checks if left child of n is available for adding a new node to. recursively tries to find the leftmost available one
	def isLeftChildAvailable(n: Node) : Boolean = { 

		if ((n.NumberOfChildren == 0)) return false;
		else if (n.LengthOfChildren() > 0)  {
			var leftChild = n.Children.apply(n.LengthOfChildren()-1);
			var diff = leftChild.NumberOfChildren - leftChild.LengthOfChildren();
			if (diff > 0) return true;
			else return isLeftChildAvailable(leftChild)
		}
		else return false;
	}

	def AddNode(curr: Node, data: Int, nChildren: Int) {
		if (curr.Data == -1) {
			curr.Data = data;
			curr.NumberOfChildren = nChildren;
			return;
		}

		if (isLeftChildAvailable(curr)) {
			var lastChild = curr.Children.apply(curr.LengthOfChildren() - 1)
			AddNode(lastChild, data, nChildren)
		}

		else { 
			//ie no leftmost node available, so we will just add the node to the current nodes' children
			var child = new Node(data, nChildren);
			curr.AppendToChildren(child);
		}

	}
}

object PostOrderTraversal {
	
	def traverse(tree: Node) {
		for (c <- tree.Children) {
			traverse(c);
		}
		println(tree.Data + " " + tree.NumberOfChildren)
	}
}

object Traverse extends App {
	var line = ""
	var tree = new Node();

	while({line = StdIn.readLine(); line != null}) { 
		
		var arr = line.split(" ");
		var value = arr.apply(0).toInt;
		var nChildren = arr.apply(1).toInt;
		
		//constructing tree by adding nodes one by one.
		AddNodeObj.AddNode(tree, value, nChildren);
	}
	//tree.pprint();
	//now let's traverse postorder
	PostOrderTraversal.traverse(tree); //tada!
}