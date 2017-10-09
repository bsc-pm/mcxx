template < int X, typename T>
struct A
{
};

template < typename T>
struct B
{
        template < int X>
        using  _A = A<X, T>;

        _A<0>   f1() { }
        A<0, T> f2() { }
};

