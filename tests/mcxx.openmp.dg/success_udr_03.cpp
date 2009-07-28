struct my_int
{
    int _n;
    my_int() : _n ( 0 ) { }
    my_int(int n) : _n(n) { }
};

void fun(my_int*, const my_int&);

#pragma omp declare reduction type(my_int) identity(constructor) operator(fun)

void g()
{
    my_int s;

#pragma omp parallel for reduction(fun : s)
    for (int i = 0; i < 100; i++)
    {
        my_int k(i);
        fun(&s, k);
    }
}
