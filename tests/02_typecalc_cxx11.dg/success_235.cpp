/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/
template<bool b, typename _Tp >
struct enable_if
{ };

template<typename _Tp>
struct enable_if<true, _Tp>
{
    typedef _Tp type;
};

template < typename T>
class A
{ };

template <>
class A<int>
{
    static constexpr bool foo() {
        return true;
    }
};

template < typename T, typename enable_if<A<T>::foo(), T>::type b2= true>
void func() {}

template < typename T, typename enable_if<A<T>::foo(), bool>::type b2 = true>
void func2() {}
