#include <iostream>
#include <cstdlib>
#include <atomic>

using namespace std;

class Node {

 public:

  int value;
  Node* next;

	Node (int v) {
    value = v;
    next = NULL;
  }

  Node (int v, Node* u) {
    value = v;
    next = u;
  }
};

class Stack {

  private: 

    struct state_t {
  		Node* top;
  		int64_t nPops;
  	};

  	std::atomic<state_t> state;

  public: 
    Stack () {
      state_t s;
      s.top = NULL;
			s.nPops = 0;			
      state.store (s);
    };
	
  int pop ();
  void push (int v);
};

int Stack::pop () {
  state_t oldState = state.load();

  if (oldState.top == NULL) {
    cout << "Stack is Empty" << endl;
    return -12;
  }
  else {
    while (1) { 
			oldState = state.load ();
			Node* oldTop = oldState.top;
			int oldTopValue = oldTop->value;
			Node* next = oldTop->next;

      /* new state */
  		state_t newState;
			newState.top = next;
			newState.nPops = oldState.nPops+1;
			
			if (state.compare_exchange_strong(oldState,newState)) {				
				return oldTopValue;
			}
		}
	}
}

void Stack::push(const int value) 
{ 
    Node *u = new Node(value); 

    while (1) { 
      state_t oldState = state.load ();
			u->next = oldState.top;
    
      /* new state */
  		state_t newState;
			newState.top = u;
			newState.nPops = oldState.nPops;			
			if (state.compare_exchange_strong(oldState, newState)) { 
				break;
			}
    }
}
