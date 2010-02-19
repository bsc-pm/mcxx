/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
extern int a[];
enum { B };

void f()
{
    int *c;
    c = a + B;
}
