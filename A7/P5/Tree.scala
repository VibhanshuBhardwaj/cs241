package parser
import scala.collection.mutable.ArrayBuffer

class Tree (prule: String) {
	val rule: String = prule;
	var children = new ArrayBuffer[Tree]();
}