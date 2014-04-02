/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    int x;
    int y;
};

void f()
{
    __builtin_offsetof(A<int>, y);
}
