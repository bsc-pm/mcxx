/*
<testinfo>
test_generator=config/mercurium
test_compile_fail=yes
</testinfo>
*/

void f(int a[const 10])
{
    a = 0;
}
