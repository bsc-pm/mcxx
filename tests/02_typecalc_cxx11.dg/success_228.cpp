/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
struct C
{
};

template < typename T>
struct B
{
    T _var;
};

template < typename T>
struct A
{
    T _a;
    decltype(_a._var) foo() { }
};

int main()
{
    A< B<C> > a;
    a.foo();
}
