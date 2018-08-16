/*
<testinfo>
test_generator="config/mercurium"
test_nolink=yes
</testinfo>
*/

namespace test1
{
    template < typename T>
        struct A;


    void foo(A<int>& var);
    void foo(A<double>& var);

    void bar(A<double> & v)
    {
        foo(v);
    }
}

namespace test2
{
    struct A
    {
        virtual A* foo() = 0;
    };

    template < typename>
    struct B {};

    template <>
    struct B<int> : public A
    {
        B<int>* foo()  {}
    };
}
