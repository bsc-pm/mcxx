/*
<testinfo>
test_generator="config/mercurium-ompss-2"
test_ENV=NANOS6_SCHEDULER=fifo
</testinfo>
*/

#include <cassert>

// There was a bug in loop normalization which converted
// a[i] into a[i + lower + lower] instead of just a[i + lower]

const int MAX_SIZE = 256, ELEMS = 10;

int main()
{
  int test[MAX_SIZE];
  int from = 5;

  for (int i = 0; i < ELEMS; i++)
  {
    test[i] = 0;
  }
  for (int i = ELEMS; i < MAX_SIZE; i++)
  {
    test[i] = -1;
  }

#pragma oss loop shared(test)
  for (int i = from; i < 10; i++)
  {
    int element = test[i];
    assert(element == 0);
  }
#pragma oss taskwait
}
