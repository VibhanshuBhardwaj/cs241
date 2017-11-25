#include <iostream>

int binsearch(int *arr, int size, int target) {
	//call helper
	int result = 0;
	result = binsearchHelper(arr, 0, size -1, target)
}

int binsearchHelper(int *arr, int l, int r, int target) {
	int index = 0;
	int newL = 0;
	int newR = 0;
	int result = 0;
	newL = l;
	newR = r;
	index = newL + (newR - newL)/2 ;
	if (newR >= newL) {
		if (*(arr + index) == target) {
			result = index;
		}
		else {
			if (*(arr + index) < target) {
				newL = index +1;
				result = binsearchHelper(arr, newL, newR, target);
			}
			else {
				newR = index - 1;
				result = binsearchHelper(arr, newL, newR, target);
			}
		}
	}
	else {
		result = -1
	}
	return result;
}

int main() {
	int arr[] = {1, 2, 5, 10, 11, 12, 10000}
	int target = 11;
	int result = binsearch(&arr, 0, 6, target);
	cout << "result " << result << endl;
}