#include <iostream>
#include <cstdlib>
#include <pthread.h>
#include <atomic>

using namespace std;

#define NTHREADS 8

struct args {
  int tid;
};

// Each thread executes this function.
void *hello(void *a)
{  
  args* myargs = (args*) a;
	int tid = myargs->tid;

  // BEGIN: CRITICAL SECTION
  cout << "Hello world! It is me, Thread " << tid << endl;
  // END: CRITICAL SECTION
	
  // Exit thread.
  pthread_exit(NULL);
}

// Mainline
int main ()
{
  pthread_t threads[NTHREADS];
	
  // Create threads each of which runs the hello function.
  for(int i=0; i < NTHREADS; i++ ){
		args* a = new args;
		a->tid = i;

		// BEGIN: CRITICAL SECTION
    cout << "main: creating thread 00" << i << endl;
    // END: CRITICAL SECTION
		
    int error = pthread_create(&threads[i], NULL, hello, (void *) a);
    if (error) {
  		// BEGIN: CRITICAL SECTION
      cout << "Error: unable to create thread," << error << endl;
      // END: CRITICAL SECTION
      exit(-1);
    }
  }

	// Exit thread.
  pthread_exit(NULL);
}
