#include <iostream>

int wain(int x, int n) {
	int result = 1;
	int temp = 1;
	int copyOfN = 1;
	copyOfN = n;
	int copyOfX = 1;
	copyOfX = x;
	if (n < 0) {
		result = -1;
	}
	else {
		if (n == 0) {
			result = 1;
		}
		else {
			while ( copyOfN > 1) {
				if (copyOfN % 2 == 0) {
					copyOfX = copyOfX * copyOfX;
					copyOfN = copyOfN/2;
				}
				else {
					temp = copyOfX * temp;
					copyOfX = copyOfX * copyOfX;
					copyOfN = (copyOfN - 1) / 2;
				}
			}
		}
	}
	result = copyOfX * temp;
	return result;

}

int main() {

}