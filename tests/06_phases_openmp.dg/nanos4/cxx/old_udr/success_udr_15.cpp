/*
<testinfo>
test_generator=config/mercurium-nanos4
test_CXXFLAGS=--variable=new_udr:0
</testinfo>
*/
#include <list>
#include <assert.h>

template <typename _T>
void f()
{
    std::list<_T> a;
}

#pragma omp declare reduction(template<typename _T> .merge : _T) 

const int N = 100;

int main (int argc, char *argv[])
{
	std::list<int> l;

	#pragma omp parallel for reduction(.merge : l)
	for ( unsigned i = 0; i < N; i++ ) {
		l.push_back(i);
	}

	assert(l.size() == N);
}
