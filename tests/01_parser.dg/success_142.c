/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

int x(void)
{
    int a = ({ int b = 3; b; });
    int b = 1;
    return a;
}
