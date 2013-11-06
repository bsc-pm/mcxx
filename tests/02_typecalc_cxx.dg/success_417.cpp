/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <int N>
struct A;

template <>
struct A<1>
{
    static int one;
};

template <>
struct A<0>
{
    static int zero;
};

template <>
struct A<-1>
{
    static int minus_one;
};

void F(void)
{
    A<true>::one;
    A<+true>::one;
    A<-true>::minus_one;

    A<false>::zero;
    A<+false>::zero;
    A<-false>::zero;

    A<(int)0.23>::zero;
    A<(int)1.03>::one;
    A<!0.23>::zero;
    A<+!0.23>::zero;
    A<-!0.23>::zero;
    A<!1.2>::zero;
    A<-!1.2>::zero;

    A<false + false>::zero;
    A<false + true>::one;
    A<false - true>::minus_one;
    A<true - true>::zero;

    A<true * true>::one;
}
