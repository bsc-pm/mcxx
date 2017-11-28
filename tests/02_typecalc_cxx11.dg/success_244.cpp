/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/

template <int... N> struct B {};

template <typename T> struct A {

  template <int... N>
      static void bar(B<N...>) {}
};

void g() {
  A<int> a;
  a.bar(B<1, 2, 3, 4>());
}
