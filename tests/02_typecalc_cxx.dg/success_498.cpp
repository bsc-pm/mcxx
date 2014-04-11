/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace N
{
    struct A
    {
        void f();
    };

    struct B : A
    {
        void g();
    };


    void B::g()
    {
        N::A::f();
        ::N::A::f();
        this->N::A::f();
        this->::N::A::f();
    }
}

