/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct A
{
    template <typename S, typename M = S[10], typename W = M>
        void foo(W&) { }
};

void f()
{
    int x[10];
    A a;
    a.foo<int>(x);
}
