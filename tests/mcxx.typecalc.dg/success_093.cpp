void g(float *);

template <typename _T>
struct A
{
    void f(_T t)
    {
        g((_T*)t);
        g(static_cast<_T*>(t));
        g(reinterpret_cast<_T*>(t));
        g(const_cast<_T*>(t));
        g(dynamic_cast<_T*>(t));

        g((float*)t);
        g(static_cast<float*>(t));
        g(reinterpret_cast<float*>(t));
        g(const_cast<float*>(t));
        g(dynamic_cast<float*>(t));
    }
};

