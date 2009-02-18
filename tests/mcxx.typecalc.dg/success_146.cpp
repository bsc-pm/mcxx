struct A
{
    template <typename _T>
        void f(const A& a) const;
};

template <typename _Q>
void A::f(const A& a) const
{
    _Q::prova(3);
}
