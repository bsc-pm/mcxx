struct A
{
    template <typename _T>
    bool operator==(_T t);
};

void f()
{
    A a;

    a == 3;
    a == 4.5f;
}
