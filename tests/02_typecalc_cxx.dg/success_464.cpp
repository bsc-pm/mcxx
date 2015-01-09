/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    void f(T*)
    {
    }

    template <typename S>
    void f(T*)
    {
    }
};
