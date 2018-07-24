#include <iostream>
#include <cstdlib>
#include <pthread.h>
#include <atomic>

using namespace std;

int main ()
{
  std::atomic<bool> flag;

  flag.store(false);
  std::cout << "flag = " << flag.load() << std::endl;
	
  bool expected = false;
  bool was_successful = flag.compare_exchange_strong(expected, true);
  std::cout	<< "* Performed compare_and_exchange" << endl;
  std::cout	<< "  flag = " << flag.load()
						<< "  ; expected = " << expected
						<< "  ; was_successful = " << was_successful
						<< std::endl;

  bool expected2 = false;
  bool was_successful2 = flag.compare_exchange_strong(expected2, true);
  std::cout	<< "* Performed compare_and_exchange" << endl;
  std::cout << "  flag = " <<	flag.load()
						<< "  ; expected = " << expected2
						<< "  ; was_successful = " << was_successful2
						<< std::endl;

}
