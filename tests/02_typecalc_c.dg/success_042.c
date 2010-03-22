/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
volatile *k; // Note the lack of a type

void g(volatile int*);

void f(void)
{
    void *m;
    g((volatile*)m); // Note again the lack of type
}
