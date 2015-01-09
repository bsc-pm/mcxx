/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

using A = int;

A a1;

struct B
{
    using A = int;
};

B::A a2;

void f(int*);

void g()
{
    f(&a1);
    f(&a2);
}
