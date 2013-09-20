/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A { };

struct B
{
    template <typename T>
        void foo(int a = 0);
};

template <>
void B::foo<A>(int a) { }

void g()
{
    B b;
    b.foo<A>();
}
