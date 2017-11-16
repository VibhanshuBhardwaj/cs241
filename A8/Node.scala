package gen
import scala.collection.mutable.ArrayBuffer

class Node(s: String, l: String = "") {
	var value = s;
	var lex = l;
	var children = new ArrayBuffer[Node]();
}