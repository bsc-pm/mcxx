/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

void f()
{
    int b[(bool)0.5 ? 1 : -1];
    b[0] = 3;
}
