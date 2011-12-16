/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

void f(void)
{
    int @reb-ref@ rr;
    int k = 1;
    int &lr = k;

    int *p = 0;

    &rr = &k;
    &rr = &lr;

    rr = 3;
    rr = k;
    rr = lr;

    k = rr;
    lr = rr;
    
    p = &rr;
    p = &lr;
}
