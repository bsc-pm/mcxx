/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename Op, typename Arg1, typename Arg2>
void f(Op op, Arg1&& arg1, Arg2&& arg2)
{
    auto m = [&]() { op(arg1, arg2); };
    m();
}

struct A
{
    A() { }
    void operator()(int, float) { }
};

void g()
{
    A a;
    f(a, 1, 2.3f);
}
