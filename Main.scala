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
		//var node = new Node(value, nChildren);
		//println("prev len children" + Children.length)
		Children = Children :+ child;
		//println("after append children len " + Children.length);

	}
	def LengthOfChildren() : Int = {
		return Children.length;
	}
	def pprint() : Unit = {
		//println("printing tree")
		if (this == null) return;
		else {
			println(this.Data)
			//println(this.Children.mkString(" "))
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

//returns the node to which next node should be added to.
object AddNodeObj {
	def AddNode(curr: Node, data: Int, nChildren: Int) {

		if (curr.Data == -1) {
			curr.Data = data;
			curr.NumberOfChildren = nChildren;
		}


		else if ((curr.LengthOfChildren() == 0) && (curr.NumberOfChildren != 0))  {
			//println("0 children of current node but there are children");
			var child = new Node(data, nChildren);
			curr.AppendToChildren(child);
			//println("just added " + child.Data + " " + child.NumberOfChildren)

		}
		else {
			var lastChild = curr.Children.apply(curr.LengthOfChildren() - 1);
			//println("last Child " + lastChild.Data + "  " + lastChild.NumberOfChildren)
			if ((lastChild.NumberOfChildren == lastChild.LengthOfChildren())) { //and both not zero?? if they're both zero do what elseif is doing
				//if last child's last child has slots, just call AddNode on last child's last child
				if (lastChild.NumberOfChildren != 0) { //remove this if block entirely to get old solution
					var lastLastChild = lastChild.Children.apply(lastChild.LengthOfChildren() - 1);
					if (lastLastChild.NumberOfChildren - lastLastChild.LengthOfChildren() > 0) {
						//println("nippy")
						AddNode(lastLastChild, data, nChildren);
						return;
					}
				}
				
				var child = new Node(data, nChildren);
				curr.AppendToChildren(child);
				//println(" last child full. just added to curr(" + curr.Data +")'s' children array" + child.Data + " " + child.NumberOfChildren)
				
			}
			else if (lastChild.NumberOfChildren != lastChild.LengthOfChildren()) { // fix this? add an or
				//println("lastChild's number of children is not same as current children")
				AddNode(lastChild, data, nChildren);
			}
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
object Main extends App {
	//println("traversing again")
	var line = ""
	var tree = new Node();
	while({line = StdIn.readLine(); line != null}) { 
		//write a while true loop that reads line number-of-children times and calls traversal on each of those lines
		var arr = line.split(" ");
		//println("line: ", line);
		var value = arr.apply(0).toInt;
		var nChildren = arr.apply(1).toInt;
		
		AddNodeObj.AddNode(tree, value, nChildren);
		//tree = nodeToBeAddedTo;

		//tree.pprint();
	}
	//now let's traverse popstorder
	tree.pprint();
	println("now PostOrderTraversal")
	PostOrderTraversal.traverse(tree)
	

}