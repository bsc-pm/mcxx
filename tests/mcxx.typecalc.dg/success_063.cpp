struct A
{
    template <typename _T>
        _T* f(_T t);

    template <typename _T>
        _T** f(_T t) const;
};

void g1(int*);

void g2(int**);

void h()
{
    A b;
    g1(b.f<int>(3));

    const A a = A();

    g2(a.f<int>(4));
}
