/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
namespace A
{
    template <typename _T1>
    struct B
    {
        enum { S = 1 };
    };

    template <typename _T2, int _N2 = B<_T2>::S >
    struct C
    {
        void foo();
    };

}

namespace D
{
    void m()
    {
        A::C<float> c;
        // c.foo();
    }
}
