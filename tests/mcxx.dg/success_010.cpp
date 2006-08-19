template <class T>
struct A
{
    typedef int P;
};

template <class T>
struct A<T*>
{
    typedef float P;
};

template <class T>
struct A<T(*)()>
{
    typedef char P;
};

template <class T, class Q>
struct A<T(*)(Q)>
{
    typedef Q P;
};

typedef int T1;
typedef A<int>::P T1;
typedef A<float>::P T1;
typedef A<char>::P T1;
typedef A<double>::P T1;
typedef A<wchar_t>::P T1;
typedef A<bool>::P T1;
typedef A<A<int> >::P T1;

typedef float T2;
typedef A<int*>::P T2;
typedef A<float*>::P T2;
typedef A<char*>::P T2;
typedef A<double*>::P T2;
typedef A<wchar_t*>::P T2;
typedef A<bool*>::P T2;
typedef A<A<int>* >::P T2;

typedef char T3;
typedef A<int(*)()>::P T3;
typedef A<float(*)()>::P T3;
typedef A<char(*)()>::P T3;
typedef A<double(*)()>::P T3;
typedef A<wchar_t(*)()>::P T3;
typedef A<bool(*)()>::P T3;
typedef A<A<int>(*)()>::P T3;

typedef A<int(*)(int)>::P T1;
typedef A<int(*)(float)>::P T2;
typedef A<int(*)(char)>::P T3;
