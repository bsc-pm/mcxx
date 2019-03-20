/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct A
{
};

template <typename T>
struct B : A
{
  void foo() override { }
};

