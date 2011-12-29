/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
void f(void)
{
    int @reb-ref@ rr;
    int k = 1;

    int *p = 0;

    &rr = &k;

    rr = 3;
    rr = k;

    k = rr;
    
    p = &rr;
}
