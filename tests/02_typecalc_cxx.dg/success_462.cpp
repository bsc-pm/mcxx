/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename __Comparator>
void f()
{
    __Comparator it;
    !bool(it.foo());
}
