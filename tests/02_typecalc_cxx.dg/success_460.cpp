/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename __Comparator, typename __Iterator>
void f()
{
    __Iterator i, j;
    !bool(__Comparator(--i, ++j));
}
