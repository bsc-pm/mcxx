/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
#include <initializer_list>

template <typename T>
T&& my_move(T&);

struct A
{
    A(int, int, int);
};

template <typename T>
void f1(T, T);

template <typename T>
void f2(T&, T&&);

template <typename T>
void f3(T&&, T&&);

void g()
{
    A a(1, 2, 3);

    f1(a, {4,5,6});
    f2(a, {4,5,6});

    f3(my_move(a), {4,5,6});
}
