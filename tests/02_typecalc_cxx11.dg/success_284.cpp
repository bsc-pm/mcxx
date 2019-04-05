/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
template<typename T, typename T1, typename T2>
void save_in_pair(T& pair,  T1& a, T2& b)
{
    pair = {a, b};
}
