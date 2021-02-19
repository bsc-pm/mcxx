/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
static const int alignSize = 64;

void foo(void)
{
  unsigned wrk[10] __attribute__ ((aligned(alignSize)));
}

static const int alignSize2 = 63;

void foo2(void)
{
  unsigned wrk[10] __attribute__ ((aligned(alignSize2 + 1)));
}
