/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
</testinfo>
*/

void f1(int &a, int &b, int &c, int &d)
{
#pragma omp task inout(a) shared(b) private(c) firstprivate(d)
    {
        c = 3;
        a++;
        b++;
        d++;
    }
#pragma omp taskwait
}

int main(int argc, char* argv[])
{
    int x, y, z, k;

    f1(x, y, z, k);

    return 0;
}
