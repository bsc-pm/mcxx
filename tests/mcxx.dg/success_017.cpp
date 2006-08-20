template <class T>
struct B
{
};

template <class T>
struct A
{
};

template <class T>
struct A<T*> : B<T>
{
    A()
        : B<T>()
    {
    }
};
