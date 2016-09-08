/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
struct B
{
    typedef int size_type;
};

template < typename T>
struct A
{
    T _a;
    typename decltype(_a)::size_type foo() { }
};

int main()
{
    B b;
    A<B> a;
    a.foo();
}
