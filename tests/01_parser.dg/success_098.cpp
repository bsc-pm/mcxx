/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
typedef int T;

T* g();

void f()
{
    if (T * p = g())
    {
    }
}
