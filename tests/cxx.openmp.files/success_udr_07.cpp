struct my_int
{
    int _n;
    my_int() : _n ( 0 ) { }
    my_int(int n) : _n(n) { }
};

void fun(my_int*, const my_int&);

#pragma omp declare reduction(fun:my_int) order(left) identity(constructor)

void g()
{
    my_int s;

#pragma omp parallel for reduction(fun : s)
    for (int i = 0; i < 100; i++)
    {
        my_int k(i);
        fun(&k, s);
    }
}
