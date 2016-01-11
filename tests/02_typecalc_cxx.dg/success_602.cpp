/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename Q1, int J2>
struct B
{
};

template <typename T1, int I1_, int I2, int I3>
struct A
{
    enum { I1 = I1_ };
    template <typename Q1>
    A& foo(const B<Q1, I1>& r);
};


template <typename T1, int I1, int I2, int I3>
template <typename Q1>
A<T1, I1, I2, I3>& A<T1, I1, I2, I3>::foo(const B<Q1, I1>& r)
/* After "::" "I1" actually means "A"'s "I1" not the template parameter "I1" */
{
}
