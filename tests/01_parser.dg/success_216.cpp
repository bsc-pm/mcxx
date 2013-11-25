/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A
{
  template <typename T>
      A(T) { }
  template <typename S>
      A(float) { }
  template <typename T>
      operator T*() { return 0; }
};

template A::A(int);
template A::A<int>(float);
template A::operator int*();
