/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
constexpr static int N = 100;

template <typename T> struct array { constexpr static const T &v = N; };

void foo() {
  array<int> a;
  static_assert(array<int>::v + 1 == 101, "");
  static_assert(a.v + 1 == 101, "");
}
