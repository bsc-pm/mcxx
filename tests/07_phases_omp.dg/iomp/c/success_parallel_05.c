/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This test does not do anything special, just tests VLA lowering*/

int main(int argc, char *argv[]) {
    int a[argc];
    argc = 4;
    a[1] = sizeof(a)/sizeof(int);
    #pragma omp parallel firstprivate(a)
    {
        a[0] = sizeof(a)/sizeof(int);
        int b[argc + 1];
    }
}
