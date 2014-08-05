/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template<typename _Tp>
struct remove_reference
{ typedef _Tp type; };

template<typename _Tp>
struct remove_reference<_Tp&>
{ typedef _Tp type; };

template<typename _Tp>
struct remove_reference<_Tp&&>
{ typedef _Tp type; };


template<typename _Tp>
typename remove_reference<_Tp>::type&& move(_Tp&& __t);

struct A
{
};

int* foo(A&&);
float* foo(const A&);

void f(A&& a, const A &&ca)
{
    int *pi = foo(move(a));
    float *pf = foo(move(ca));
}
