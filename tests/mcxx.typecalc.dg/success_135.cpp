template <typename _T>
struct A 
{ 
    A(const _T&);
};

enum E
{
    E1, E2, E3
};

typedef E E_t;

template<typename _Tp>
bool
operator==(const A<_Tp>& __x, const _Tp& __y);


static const E_t M_E1 = E1;

void g()
{
    E e1;

    e1 == M_E1;
}
