template <class T>
struct A
{
    void f(T t);
};

template <>
void A<int>::f(int t)
{
}
