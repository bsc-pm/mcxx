/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct A
{
    float *m;
};

int f(void)
{
    struct A a;
    return g(a) + g(a, a);
}
