/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T, typename S>
struct A {
    typedef T Type1;
    typedef S Type2;
};

template <typename T>
using B = A<T, T*>;

template <typename T>
using C = typename A<T, T*>::Type2;

typedef int *T1;
typedef B<int>::Type2 T1;
typedef C<int> T1;

template <typename T>
void g(B<T>*);

void f()
{
    B<int> *b;

    g(b);

    C<int> c;
    int *p;

    c = p;
}
