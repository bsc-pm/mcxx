template <typename _T>
struct A
{
};

// template <typename _T>
// void f(const A<const _T>& p)
// {
// }

template <typename _T>
void f(const A<_T>& p)
{
}

typedef A<int> C;

void g(C& c)
{
    f(c);
}
