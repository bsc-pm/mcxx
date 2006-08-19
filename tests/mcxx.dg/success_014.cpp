template <class T>
struct A
{
    struct B : public T
    {
        B() :
            T(3)
        {
        }
    };
};

struct C 
{
    C(int n) { }
};

struct D : public A<C>
{
};
