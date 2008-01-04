#include <typeinfo>

// This test might fail not because of the thing it is testing

struct A
{
};

const char* f1()
{
    return typeid(A).name();
}

const char* f2()
{
    A a;
    return typeid(a).name();
}
