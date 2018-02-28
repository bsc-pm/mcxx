/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
test_nolink=yes
</testinfo>
*/

class A
{
    public:
        int _x;
        A(int x) : _x(x) {}
};

int main() {
    A a(2);
    #pragma omp task
    {
        a;
    }
}
