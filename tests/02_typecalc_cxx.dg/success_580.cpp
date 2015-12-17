/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <class T>
struct A {
        enum { B = 256 };

        enum { C = B + 1 };
        T D[C];
};

void f()
{
    A<int*> a;

    int *p;
    a.D[1] = p;
}
