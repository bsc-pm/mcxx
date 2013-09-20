/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A;

template <> struct A<int>;

template <typename T>
struct B
{
};

template <typename T>
void f(A<T> &a);

template <>
void f(A<int> &a);

template <typename T>
void f(B<T> &b);
