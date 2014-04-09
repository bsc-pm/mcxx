/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    template <typename F>
        A& add(const char*, F f);
};

struct B
{
    int myFoo(int);
    float myBar(int);
};

void g()
{
    A<int>()
        .add("myFoo", &B::myFoo)
        .add("myBar", &B::myBar);
}
