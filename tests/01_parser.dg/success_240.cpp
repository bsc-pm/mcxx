/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename T>
struct A
{
    template <typename S>
        void f(T, S)
        {
        }
};

template <>
template <typename Q>
void A<int>::f(int, Q)
{
}
