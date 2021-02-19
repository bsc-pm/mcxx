/*
<testinfo>
test_generator=config/mercurium
test_CXXFLAGS="-fopenmp"
</testinfo>
*/
#include <cstdio>

void foo(int N)
{
#pragma omp parallel for
  for (int i = 0; i < N; i++)
  {
    printf("%d\n", i);
  }
}


// We know this is not valid, but also checks that we convert 'int i(0)' into
// 'int i = 0' in this case.
void foo2(int N)
{
#pragma omp parallel for
  for (int i(0); i < N; i++)
  {
    printf("%d\n", i);
  }
}
