/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

#if ((__GNUC__ < 4) \
        || (( __GNUC__ == 4) && __GNUC_MINOR__ < 6))
    #define IGNORE_TEST
#endif

#ifndef IGNORE_TEST
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
#endif
