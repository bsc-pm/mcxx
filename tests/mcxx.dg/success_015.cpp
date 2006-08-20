template <class T>
struct A
{
};

template <class T>
struct B
{
    A<T> f();
};

template <class T>
A<T> B<T>::f()
{
    A<T> t;
    return t;
}

void g()
{
    B<int> b;
    A<int> a;
    a = b.f();
}
