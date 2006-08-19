template <class T>
struct A
{
};

template <class T>
struct B : public A<T>
{
    typedef A<T> _A;

    B() :
        _A()
    {
    }
};

struct C : B<int>
{
};
