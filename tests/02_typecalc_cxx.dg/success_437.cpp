/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/
struct A
{
  explicit A(int* p=0) { }
};

struct B
{
    A a;
};

B b;
