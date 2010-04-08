/*
<testinfo>
test_generator=config/mercurium-nanos4
</testinfo>
*/
#include <iostream>
#include <limits>

#pragma omp declare reduction (template<typename _T> std::min : _T) identity(std::numeric_limits<_T>::max())
#pragma omp declare reduction (template<typename _T> std::max : _T) identity(std::numeric_limits<_T>::min())

const int MOD = 10;
const int N = 101;

int main ()
{
	int a[N];
	int min,max;

	for ( unsigned i = 0; i < N; i++ ) a[i] = i+MOD;

	min = std::numeric_limits<int>::max();
	max = std::numeric_limits<int>::min();
	
	#pragma omp parallel for reduction(std::min : min) reduction(std::max : max)
	for ( unsigned i = 0; i < N; i++ ) {
		min = std::min(min, a[i]);
		max = std::max(max, a[i]);
	}

	std::cout << "min=" << min << " max=" << max << std::endl;
}
