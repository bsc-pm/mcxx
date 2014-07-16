/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct Foo
{
    struct Bar
    {
        template <typename S1, typename S2>
            struct Template
            {
                void g();
            };
    };
};

template <typename T1, typename T2>
struct A
{
    private:
    int x;
    friend struct Foo::Bar::Template<T1, T2>;
};

template <typename S1, typename S2>
void Foo::Bar::Template<S1, S2>::g()
{
    A<S1, S2> a;
    a.x = 3;
}

void f()
{
    Foo::Bar::Template<int, float> a;
    a.g();
}
