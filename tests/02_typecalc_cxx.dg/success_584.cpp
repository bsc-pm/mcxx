/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

enum E
{
    V1, V2
};

double* h(const E& e);
float* h(E& e);

struct A
{
    E f(int x, E e1) const
    {
        double *d = h(x < 1 ? moo.foo : e1);
    }

    union U
    {
        E foo;
    } moo;
};
