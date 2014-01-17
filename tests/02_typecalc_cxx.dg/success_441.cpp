/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/

void f(int);

namespace A
{
    using ::f;
}

namespace B
{
    using ::A::f;
    using B::f;

    template <typename T>
    void g(T* t);

    void foo()
    {
        B::g(B::f);
    }

}
