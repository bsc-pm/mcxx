/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
namespace N
{
    template <typename T>
        T* make_ptr();

    template <typename T>
        struct A
        {
            template <typename Q, typename S = decltype(make_ptr<Q>())>
            void foo(Q);
        };
}

void g()
{
    N::A<int> a;
    a.foo(1);
}
