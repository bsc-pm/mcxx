/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/
#include <list>
#include <assert.h>

template <typename _T>
void f()
{
    std::list<_T> a;
}

#pragma omp declare reduction(template<typename _T> std::list<_T>::merge : std::list<_T>) 

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
