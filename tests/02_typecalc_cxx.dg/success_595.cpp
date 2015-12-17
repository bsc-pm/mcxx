/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A;

template <typename T, typename Q>
A<T Q::*> foo();                      // (1)

template <typename T, typename Q>
struct A<T Q::*>                      // (2)
{
    typedef T X;
};

struct B
{
   int foo();
};

typedef int I();
typedef A< int (B::*)() >:: X I;   // (3)
