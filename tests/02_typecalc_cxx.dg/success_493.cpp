/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    union
    {
        T* x;
        T** y;
        T*** z;
    } u;
};

void g()
{
    int *x = 0;
    int **y = 0;
    int ***z = 0;

    A<int> a;

    a.u.x = x;
    a.u.y = y;
    a.u.z = z;
}
