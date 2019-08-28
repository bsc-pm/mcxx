/*
<testinfo>
test_generator=config/mercurium-ompss-2
</testinfo>
*/

#include <assert.h>

void array_add(int *c, int *a, int *b, int N)
{
    // Basic form without anything very interesting in the dependences
#pragma oss taskloop grainsize(1)
    for (int i = 0; i < N; i++)
    {
        c[i] += a[i] + b[i];
    }
#pragma oss taskwait
}

int a[] = { 1, 2, 3, 4 };
int b[] = { 5, 6, 7, 8 };
int c[] = { -1, -1, -1, -1 };
const int ref[] = { 5, 7, 9, 11 };

const int SIZE = sizeof(a) / sizeof(*a);

int main(int argc, char *argv[])
{
  array_add(c, a, b, SIZE);

  int i;
  for ( i = 0; i < SIZE; i++)
  {
    assert(c[i] == ref[i] && "Invalid value");
  }

  return 0;
}
