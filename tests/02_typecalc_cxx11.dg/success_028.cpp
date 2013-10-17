/*
<testinfo>
test_generator="config/mercurium"
test_CXXFLAGS="-std=c++11"
</testinfo>
*/

template <typename ...T, typename ...S>
void f() throw (S..., T...)
{
}
