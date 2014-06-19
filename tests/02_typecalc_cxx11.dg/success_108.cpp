/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
T* g(T);

template<typename T>
auto f(T x) -> decltype(g(x));

typedef int* T1;
typedef decltype(f(3)) T1;

typedef double* T2;
typedef decltype(f(3.4)) T2;

typedef float* T3;
typedef decltype(f(3.4f)) T3;
