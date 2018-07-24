#include <iostream>
#include <cstdlib>

using namespace std;

template <typename T>
class Node {
 private: 
  T value;
  Node* next;

 public:
  Node (T v) {
    value = v;
    next = NULL;
  }

  Node (T v, Node* u) {
    value = v;
    next = u;
  }

  T getValue () {
    return value;
  };
  
  Node* getNext () {
    return next; 
  };

  void setNext (Node u) {
    *next = u;
  };

};

template <typename T>
class Stack {

 private: 
  Node<T>* top;

 public: 
  Stack () {
    top = NULL;
  };

  T pop ();
  void push (T v);
};

template <typename T>
T Stack<T>::pop () {
  if (top == NULL) {
    cout << "Stack is Empty" << endl;
    return -12;
  }
  else {
    int oldTop = (*top).getValue();   
    top = (*top).getNext ();
    return oldTop;
  }
}

template <typename T>
void Stack<T>::push (T value) {
  top = new Node<T> (value,top); 
  return ;  
}
