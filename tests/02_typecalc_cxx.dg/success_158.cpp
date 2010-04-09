/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename _T>
struct A
{
};

void f(A<int>* a)
{
    a->A<int>::~A();
}

void f2(A<int>& b)
{
    b.A<int>::~A();
}

void f3(A<int>& c)
{
    typedef A<int> T;

    c.T::~A();
}
