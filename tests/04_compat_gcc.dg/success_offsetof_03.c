/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct a
{
    int x[4];
};

void f()
{
    int test1[__builtin_offsetof(struct a, x[0]) == 0 ? 1 : -1];
    int test2[__builtin_offsetof(struct a, x[3]) == sizeof(int) * 3 ? 1 : -1];
    *test1 = *test2 = 0;
}
