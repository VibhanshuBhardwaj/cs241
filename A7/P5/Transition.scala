package parser

class Transition (pfrom: String, pnextToken: String, pto: String, ptType: String  ) {
	var from: String = pfrom;
	var to: String = pto;
	var tType: String = ptType;
	var nextToken: String = pnextToken;
}