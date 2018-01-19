/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
test_compile_fail=yes
</testinfo>
*/

#pragma omp target device(cuda) ndrange(1, 1, 1)
#pragma omp task
__global__ void foo();

#pragma omp target device(cuda) ndrange(1, 1, 1)
#pragma omp task
void bar();

int main() {
    foo();
    bar();
}
