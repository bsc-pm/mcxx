/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T>
void f(const T&) noexcept(sizeof(T) > 1)
{
}

template <bool B>
struct A
{
    static int *x;
};

template <>
struct A<true>
{
    static float *x;
};

int main(int argc, char *argv[])
{
    float *pf;
    int *pi;

    char c;
    pi = A<noexcept(f(c))> :: x;

    int i;
    pf = A<noexcept(f(i))> :: x;

    return 0;
}
