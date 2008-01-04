template <typename _T>
_T& operator+(_T&);

struct A
{
};

void f()
{
    A a;

    +a;
}
