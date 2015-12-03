/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
int foo(void)
{
  const int cs[2L] = {[0] = 0, [1] = 0};
  struct  point
  {
    int x;
    int y;
  };
  static const struct point zero = {.x = 0, .y = 0};

  return !cs[0] && !zero.x;
}
