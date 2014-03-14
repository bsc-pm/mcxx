/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T>
struct A
{
    static void g(A&);

    void f1(A& x, A& y) noexcept(noexcept(g(x)) && noexcept(g(y))) { }

    void f2(A& x, A& y) noexcept(noexcept(g(x)) && noexcept(g(y)));
};
