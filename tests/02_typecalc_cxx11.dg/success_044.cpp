/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

typedef int T;

typedef int &T_lref;
typedef int &&T_rref;

typedef T_lref &T_lref_lref;

typedef T_lref &&T_lref_rref;

typedef T_rref &&T_rref_rref;

typedef T_rref &T_rref_lref;

template <typename T>
struct A { };

template <typename T>
struct A<T&> { typedef int* Type; };

template <typename T>
struct A<T&&> { typedef float* Type; };

void g()
{
    float *pf;
    int *pi;

    {
        A<T_lref_lref>::Type p;
        p = pi;
    }

    {
        A<T_lref_rref>::Type p;
        p = pi;
    }

    {
        A<T_rref_lref>::Type p;
        p = pi;
    }

    {
        A<T_rref_rref>::Type p;
        p = pf;
    }
}
