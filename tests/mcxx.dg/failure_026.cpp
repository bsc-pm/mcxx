template <class T, class Q>
struct A 
{
    typedef int T;
};

template <class T, class Q>
struct A<T*, Q> 
{
    typedef int T;
};

template <class T, class Q>
struct A<T, Q*> 
{
    typedef int T;
};

void f()
{
    A<int*, float*>::T k;
}
