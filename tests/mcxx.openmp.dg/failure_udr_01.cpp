#include <string>
#include <iostream>
#include <sstream>
#include "omp.h"

using namespace std;

string empty;

template <class T>
inline string toString (const T& t)
{
   stringstream ss;
   ss << t;
   return ss.str();
}

#pragma omp declare reduction type(string) operator(+) identity(constructor)
#pragma omp declare reduction type(string) operator(+) identity(empty)

int main ()
{
string hello;

#pragma omp parallel reduction(+:hello)
	hello = "I'm thread " + toString<int>(omp_get_thread_num()) + "\n" ;

cout << hello;

}
