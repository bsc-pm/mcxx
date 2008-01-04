struct A
{
    typedef unsigned short mask;
};

struct C { };

template <typename _T>
struct B : public C, public A
{
    void f(mask a);
};
