/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/

template<typename _Scalar>
class Matrix
{
    public:
        Matrix(int a) {}
        void setZero() {}
};

template < typename T>
struct X
{
    void foo() {
        Matrix<T> buffer( (int()) );
        buffer.setZero();
    }
};

int main() {
    X<int> var;
    var.foo();
}
