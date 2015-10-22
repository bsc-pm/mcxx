/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

void f(void *p)
{
    __sync_val_compare_and_swap(&p, p, p);
}
