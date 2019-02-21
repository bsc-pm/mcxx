/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename _B1, typename _B2> struct AND {};

template <typename _Tp, typename... _Args> struct IsX {};

template <typename... _Elements> struct B {
  using type = AND<IsX<_Elements, _Elements, _Elements>...>;
};

template <typename T1, typename T2> struct A : B<T1, T2> {};

int main() {
  using T = AND<IsX<int, int, int>, IsX<float, float, float>>;
  using T = A<int, float>::type;
}
