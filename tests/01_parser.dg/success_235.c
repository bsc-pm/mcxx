/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

void f(int a, int b)
{
    if (a < 1)
    {
    }
    else
    {
        typedef int (*foo)[a/b];
        foo c;

        (foo)c;
    }
}
