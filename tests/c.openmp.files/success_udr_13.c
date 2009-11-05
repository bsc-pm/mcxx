int imax(int a, int b) {
    return a > b ? a : b;
}

int main(int argc, char **argv) {
#pragma omp declare reduction(imax:int) identity( (-2147483647 - 1) )

    int k, v[100], m;
#pragma omp parallel for reduction(imax : m)
    for (k = 0; k < 100; k++)
    {
        m = imax(m, v[k]);
    }
    return 0;
}
