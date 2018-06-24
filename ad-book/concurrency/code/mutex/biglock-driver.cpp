#include <iostream>
#include <cstdlib>
#include <pthread.h>
#include <atomic>

using namespace std;

#define NTHREADS 8
#define NPUSHPOP 2

std::atomic<bool> BigLock;


struct args {
  int tid;
};

/* Take BigLock */
void takeBigLock () {
  while (1) {
    bool flag = false;
    if (BigLock.compare_exchange_strong(flag,true)) {
		  return;
		}		
	}
}

/* ReleaseBig BigLock */
void releaseBigLock () {
  while (1) {
    bool flag = true;
    if (BigLock.compare_exchange_strong(flag,false)) {
		  return;
		}		
	}
}

void *hello(void *a)
{  
  args* myargs = (args*) a;
	int tid = myargs->tid;

//	takeBigLock ();
  cout << "Hello world! It is me, Thread " << tid << endl;
	releaseBigLock ();

  pthread_exit(NULL);
}

int main ()
{
  pthread_t threads[NTHREADS];
  BigLock.store(false);
	
 	int rc;
  for(int i=0; i < NTHREADS; i++ ){
		args* a = new args;
		a->tid = i;
		
  	takeBigLock ();
    cout << "main: creating thread 00" << i << endl;
    releaseBigLock ();
    int error = pthread_create(&threads[i], NULL, hello, (void *) a);
    if (error) {
	  	takeBigLock ();
      cout << "Error: unable to create thread," << error << endl;
      releaseBigLock ();
      exit(-1);
    }
  }
  pthread_exit(NULL);
}
