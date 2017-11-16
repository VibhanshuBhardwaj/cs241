package gen
import scala.collection.mutable.ArrayBuffer

class Node(s: String, r: String, l: String = "") {
	var value = s;
	var rule = r;
	var lex = l;
	var children = new ArrayBuffer[Node]();
}