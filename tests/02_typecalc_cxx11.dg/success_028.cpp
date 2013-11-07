/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename ...T, typename ...S>
void f() throw (S..., T...)
{
}
