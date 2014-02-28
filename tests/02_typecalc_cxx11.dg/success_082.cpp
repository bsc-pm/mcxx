/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename Q>
struct C {
    enum { value = sizeof(Q) };
};

template <typename T , int P = C<T>::value>
struct A { };

template <typename ...S>
struct B
{
    void f(A<S>&...);
};

int main(int argc, char *argv[])
{
    A<int> a1;
    A<double> a2;

    B<int, double> b;

    b.f(a1, a2);

    return 0;
}
