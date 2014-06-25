/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A {
};

template <typename T>
struct A<T()> {
    int x;
};

template <typename T>
void f()
{
   A<T()> a;

   a.x = 3;
}

void g()
{
    f<int>();
}
