/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename T>
class B
{
  static int foo;
};

template<> int B<int>::foo;
int bar = 0;
template<> int B<int>::foo(bar);
