/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct B
{
    enum E { V = sizeof(T) };
};

template <typename T>
struct A : B<T>
{
};

template <typename T>
struct C
{
    C(const char*);
};

void g()
{
    C<A<int>::E > e("hola");
}

