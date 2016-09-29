/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
void zas();
int bar(int x);
void  foo()
{
    zas == zas;
    bar == bar;
    zas == (void(*)()) bar;
    bar == (int(*)(int)) zas;
}
