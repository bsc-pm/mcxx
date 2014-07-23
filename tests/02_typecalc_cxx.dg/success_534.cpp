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

struct A
{
    template <typename, typename>
        friend struct Foo::Bar::Template;
    private:
    int x;
};

template <typename S1, typename S2>
void Foo::Bar::Template<S1, S2>::g()
{
    A a;
    a.x = 3;
}

void f()
{
    Foo::Bar::Template<int, float> a;
    a.g();
}
