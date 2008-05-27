
int fib(int a)
{
    int n1 = 0, n2 = 0;
#pragma omp task if (a > 50)
    {
        n1 = fib(a - 1);
        n2 = fib(a - 2);
    }

    return n1;
}
