template <typename _T>
struct A
{
    _T t;
};

template class A<float>;
extern template class A<int>;
