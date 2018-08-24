/*
<testinfo>
test_generator="config/mercurium run"
</testinfo>
*/
#include <stdlib.h>
#include <assert.h>

static int my_function()
{
  return 42;
}

static void check(size_t s)
{
  assert(s == sizeof(double[42][42]) && "Invalid size computed");
}

static void check2(size_t s)
{
  assert(s == sizeof(double[84][43]) && "Invalid size computed");
}

int main(int argc, char **argv)
{
  int nt = 1;
  nt = my_function();

  check(sizeof(double[nt][nt]));
  check2(sizeof(double[nt*2][nt+1]));

  return 0;
}
