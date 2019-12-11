/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T>
void goo(T) { }
void foo(int t)
{
  int m;
  goo([m](int x) { return x + m; });
}

