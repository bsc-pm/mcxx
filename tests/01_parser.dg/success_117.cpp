/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template<typename _T >
struct A
{
        _T i;
};
template<typename _T >
struct B : A<_T>
{
        A<_T>::i;
};
B<int> b;

