/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

#if defined(__GNUC__)
  #if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 9)

template <typename T>
void f(T t)
{
}

struct A
{
    void g()
    {
        A*&& a = this;
    }
};

  #endif
#endif
