#include <bits/stdc++.h>
using namespace std;

int main () {
	int nAlphabets;
	
	cin >> nAlphabets;
	vector <string> ALPHABETS(nAlphabets);
	//populating ALPHABETS
	for (int i = 1; i<=nAlphabets; i++) {
		string readAlphabet;
		cin >> readAlphabet;
	//	cout << "readAlphabet " << readAlphabet << endl;
		ALPHABETS.push_back(readAlphabet);
	}


	int nStates;
	cin >> nStates;
	//cout << "nStates " << nStates << endl;
	vector <string> STATES(nStates);
	for (int i = 1; i<=nStates; i++) {
		string readState;
		cin >> readState;
	//	cout <<"readState, i " << readState << " " << i << endl;
		STATES.push_back(readState);
	}

	string START_STATE;
	cin >> START_STATE;

	int nFinalStates;
	cin >> nFinalStates;
	vector <string> FINAL_STATES(nFinalStates);
	for (int i =1; i <= nFinalStates; i++) {
		string readState;
		cin >> readState;
	//	cout << "readFinalState: " << readState <<endl;
		FINAL_STATES.push_back(readState); 
	}
	//cout << "final states completed" << endl;
	int nTransitions;
	cin >> nTransitions;
	//cout << "nTransitions: " << nTransitions << endl;
	map <string, vector <pair <string, string>>> TRANSITIONS;
	//a map 
	for (int i = 1; i <= nTransitions; i++) {
		string transition;
		getline(cin, transition);
		if (transition.length() < 2) {
			i--;
			continue;
		}
		//cout << "getline has read this transition " << transition << endl;
		bool fws =  false;
		bool sws = false;
		string currState;
		string input;
		string nextState;
		for (int j =0; j< transition.length(); j++) {
			if (!fws && ! sws) {
				if (transition[j] == ' ') {
					fws = true;
				}
				else {
					currState += transition[j];
				}
			}
			else if (fws && !sws) {
				if (transition[j] == ' ') {
					sws = true;
				}
				else {
					input += transition[j];
				}
			}
			else {
				nextState += transition[j];
			}
		}
		if (TRANSITIONS.count(currState) == 0) {//dne in map
			vector <pair <string, string>> v;
			pair <string, string> p(input, nextState);
			v.push_back(p);
			TRANSITIONS[currState]= v;
		}
		else {//it exists in map
			pair <string, string> p(input, nextState);
			TRANSITIONS[currState].push_back(p);
			//TRANSITIONS[currState] = newVec;
		}
		//cout << "one transition added :" << transition << endl;

	}
	string inputLineToDFA;
	while (getline(cin, inputLineToDFA)) {
		//cout << "read input " << inputLineToDFA << endl;
		string currState = START_STATE;
		vector <string> inputAlphabets;
		for (int i =0; i < inputLineToDFA.length(); i++) {
			auto possibleNextStates = TRANSITIONS[currState];
			//cout << "currState " << currState << endl;
			for (auto j: possibleNextStates) {
			//	cout << j.first << " "<< j.second << endl;
			}
			if (inputLineToDFA[i] == ' ') continue;
			string alph = "";
			alph+= inputLineToDFA[i];
			//cout << "alph " << alph << endl;
			for (int j = 0; j < possibleNextStates.size(); j++) {
				if (alph == possibleNextStates.at(j).first) {
					string nextPotentialState = possibleNextStates.at(j).second;
					currState = nextPotentialState;
				//	cout << "updated state" << endl;
					break;
				}
			}
			
		}
		auto it = find(FINAL_STATES.begin(),FINAL_STATES.end(), currState);
		if (it == FINAL_STATES.end()) {
			cout << "false" << endl;
		}
		else {
			cout << "true" << endl;
		}


	}
}