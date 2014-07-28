/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct A
{
    int x;
    float b[2];
};

void f()
{
    A a;

    a = {1, 2.4f, 5.6f};
}
