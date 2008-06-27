int kaka, a;
void f(void)
{
#pragma omp critical(kaka)
    {
        a = 3;
    }

    kaka = 3;
}
