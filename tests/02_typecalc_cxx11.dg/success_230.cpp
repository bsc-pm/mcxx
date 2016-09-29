/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
struct A
{
    A() {}
    A(A&& a) {}
    A(const A&)  = delete;
};

A foo1()
{
    A kk = A();
    return kk;
}

template < typename T>
struct B
{
    B() {}
    B(B<T>&& a) {}
    B(const B<T>&) = delete;
    B& operator=(const B<T>&) = delete;
};

B<int> foo2()
{
    B<int> kk = B<int>();
    return kk;
}
