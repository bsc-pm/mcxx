/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
void f(int x)
{
    if ((union { int a; float b; }){x}.b > 3.4f)
    {
    }
}
