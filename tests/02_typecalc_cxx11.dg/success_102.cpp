/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

#include <initializer_list>

struct A
{
    int x;
    int y;
    int z;
};

struct B { B(int, int, int); };

template <typename T> void f1(std::initializer_list<T> a);

void f2(A);

void f3(std::initializer_list<float> a);

void f4(B);
void f5(B&&);

void f6(int (&&)[3]);

void g()
{
    f1({1, 2, 3});
    f2({1, 2, 3});
    f3({1, 2, 3});
    f4({1, 2, 3});
    f5({1, 2, 3});
    f6({1, 2, 3});
}
