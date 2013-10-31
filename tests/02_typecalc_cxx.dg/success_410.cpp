/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T, typename S>
struct A
{
    void f(void)
    {
        union
        {
            int a[sizeof(T)];
            S b;
        } tmp;

        tmp.a[2] = 3;
    }
};
