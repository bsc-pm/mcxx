/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T, int A, int B>
void f(const T& t)
{
    t.template foo<A, B>(1, 2, 3);
}

struct C
{
    template <int A, int B>
        void foo(int, int , int) const;
};

void h()
{
    C c;

    f<C, 10, 20>(c);
}
