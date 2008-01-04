
template <typename _T>
struct A
{
    typedef _T K;
    void f(_T t)
    {
        called(3);
    }

    K called;
};

