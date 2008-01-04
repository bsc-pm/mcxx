struct A
{
    template <typename _T>
    A& operator+();
};

void f()
{
    A a;

    a.operator+<int>();
}
