#include <iostream>
#include <cstdlib>
#include "stack.h"

using namespace std;


int main () {
  Stack<int>* S = new Stack<int> (); 
  int i; 

  cout << "Stack driver..." << endl;

  (*S).pop ();
  (*S).push (1);
  cout << "Push 1" << endl;
  (*S).push (2);
  cout << "Push 2" << endl;
  (*S).push (3);
  cout << "Push 3" << endl;
  i = (*S).pop ();
  cout << "Pop returns " << i << endl;
  i = (*S).pop ();
  cout << "Pop returns " << i << endl;
  i = (*S).pop ();
  cout << "Pop returns " << i << endl;
  i = (*S).pop ();
  cout << "Pop returns " << i << endl;
  return 1;
}
