/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct A
{
    template <typename _T>
        operator _T* ();
};

void f(void)
{
    int *p;
    A a;
    p = a;
}
