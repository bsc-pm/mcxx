/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

void f(void)
{
    __attribute__((vector_size(16))) unsigned a, b;
    __attribute__((vector_size(16))) unsigned int c;

    1 + b;
    c = 1 > b;
    c = a > b;
}
