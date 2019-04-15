/*
<testinfo>
test_generator="config/mercurium-cxx11 run"
</testinfo>
*/
template < typename T>
struct C
{
    static const T N1 = 1;
    static const T N2;

     static constexpr T K1 = 1;
};

template < typename T>
const T C<T>::N1;

template < typename T>
const T C<T>::N2 = 2;

template < typename T>
constexpr T C<T>::K1;


const int *p_N1 = &C<int>::N1;
const int *p_N2 = &C<int>::N2;

const int *p_K1 = &C<int>::K1;

int main() {
}
