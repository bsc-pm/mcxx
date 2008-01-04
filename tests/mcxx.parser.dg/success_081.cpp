template <typename _T>
struct A
{
};


template <typename _T,
         bool b = A<_T>::__value >
struct B
{
    typedef _T Q;
};

template <typename _T>
struct C
{
    void g(typename B<_T>::Q);
};

template <typename _T>
void C<_T>::g(typename B<_T>::Q)
{
}
