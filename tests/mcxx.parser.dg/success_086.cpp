struct A
{
    template <typename _T>
        operator _T();
    template <typename _T>
        operator _T*();
};

template <>
A::operator int* ()
{
}
