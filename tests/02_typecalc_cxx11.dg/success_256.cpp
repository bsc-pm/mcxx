/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/
class A
{
    public:
        A(const double& __indata) noexcept;
        A(const double&&) = delete;
        operator const double&() const noexcept;
};

template<typename T>
class B
{
    const T _var;
};


const B<A> bar(const A& func);

void nrandn(int n)
{
    double p;
    A a(p);
    B<A> p2 = bar(a);
}
