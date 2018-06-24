#include <iostream>
#include <cstdlib>
#include <pthread.h>
#include <unistd.h>
#include <atomic>
#include "nb-stack.h"

using namespace std;

int NTHREADS = 8;
int NPUSHPOP = 5000;

std::atomic<bool> BigLock;


struct args {
  int tid;
	Stack* s;
};

/* spin lock */
void takeLock () {
  while (1) {
    bool flag = false;
    if (BigLock.compare_exchange_strong(flag,true)) {
		  return;
		}		
	}
}

/* spin lock */
void releaseLock () {
  while (1) {
    bool flag = true;
    if (BigLock.compare_exchange_strong(flag,false)) {
		  return;
		}		
	}
}

void *pushPop(void *a)
{  
  args* myargs = (args*) a;
	int tid = myargs->tid;
  Stack* sharedStack = myargs->s;

	takeLock ();
  cout << "Hello world! It is me, 00" << tid << endl;
	releaseLock ();
	
  for (int i = 0; i < NPUSHPOP; ++i) {
    int j = NPUSHPOP * tid + i;

		takeLock ();
    sharedStack->push (j);
  	releaseLock ();

    takeLock ();
//    cout << "Thread " << tid << " pushed " << j << "+" << endl;
    releaseLock ();

//  	usleep (1000);

    takeLock ();
    int k = sharedStack->pop ();
  	releaseLock ();
		
    takeLock ();
//    cout << "Thread " << tid << " popped " << k << "-" << endl;
    releaseLock ();
//		usleep (1);
  }

  pthread_exit(NULL);
}

int main (int argc, char* argv[])
{
  pthread_t threads[NTHREADS];
  Stack* sharedStack = new Stack (); 
  BigLock.store(false);
	
  // Parse arguments

	if (argc == 3) {
			NTHREADS = atoi(argv[1]);
			NPUSHPOP = atoi(argv[2]);
	}
	else if (argc == 2) {
			NTHREADS = atoi(argv[1]);
	}
  cout << "Executing command: " << argv[0] << " " 
			 << "Num Threads: " <<  NTHREADS << " "
    	 << "Num Push and Pops: " << NPUSHPOP;
	
  // Mainline
	for(int i=0; i < NTHREADS; i++ ){
		args* a = new args;
		a->tid = i;
		a->s = sharedStack;
		
  	takeLock ();
    cout << "main: creating thread 00" << i << endl;
    releaseLock ();
    int error = pthread_create(&threads[i], NULL, pushPop, (void *) a);
    if (error) {
      cout << "Error: unable to create thread," << error << endl;
      exit(-1);
    }
  }
  pthread_exit(NULL);
}
