template <class T>
struct A { };

template <class T>
struct B : A<T> {
    typedef A<T*> c;

    B() :
        c()
    {
    }
};

struct C : B<int>
{
};
