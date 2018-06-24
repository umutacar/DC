#include <iostream>
#include <cstdlib>

using namespace std;

// A stack node
class Node {
  public:

	int value;
  Node* next;

  // Node constructor
  Node (int v, Node* u) {
    value = v;
    next = u;
  }
};

class Stack {
  public:
	
	// Top of the stack
  Node* top;

  // Stack constructor
  Stack () {
    top = NULL;
  };

  // Pop the top item and return
  int pop ();

	// Push the provided item on top of the stack
  void push (int v);
};

int Stack::pop () {
  if (top == NULL) {
    cout << "Stack is Empty" << endl;
    return -12;
  }
  else {
    int oldTop = top->value;
    top = top->next;
    return oldTop;
  }
}

void Stack::push (int v) {
  top = new Node (v,top); 
  return ;  
}
