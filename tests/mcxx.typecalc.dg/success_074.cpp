template <typename _T>
struct A
{
};

template<>
struct A<int>
{
    void f()
    {
        this->g();
    }

    void g()
    {
    }
};
