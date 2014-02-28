/*
<testinfo>
test_generator="config/mercurium-cxx11 run"
</testinfo>
*/

template <typename T>
struct A { };

template <typename ...S>
struct B
{
    void f(A<S>&...) { }
};

int main(int argc, char *argv[])
{
    A<int> a1;
    A<float> a2;

    B<int, float> b;

    b.f(a1, a2);

    return 0;
}
