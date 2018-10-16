/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

#include <assert.h>
#include <stdint.h>
#include <omp.h>

struct A
{
    int *p;
    int N;
};

void neutral_init(A *var)
{
    for (int i = 0; i < var->N; ++i)
        var->p[i] = 0;
}

void comb(A* out, A *in)
{
    for (int i = 0; i < out->N; ++i)
        out->p[i] += in->p[i];
}

void init(A* priv, A *orig)
{
    priv->p = new int[orig->N];
    priv->N = orig->N;
    neutral_init(priv);
}

#pragma omp declare reduction(my_add: struct A : comb(&omp_out, &omp_in)) initializer(init(&omp_priv, &omp_orig))
int main(int argc, char *argv[]) {
    A var;

    var.N = 10; //! Assume that this value is something we don't know at compile time!
    var.p = new int[var.N];
    neutral_init(&var);

    int mat[20][10];
    for (int i = 0; i < 20; ++i)
        for (int j = 0; j < 10; ++j)
            mat[i][j] = (j*20) + i;

    #pragma omp parallel for reduction(my_add: var) collapse(2)
    for (int i = 0; i < 20; ++i)
        for (int j = 0; j < 10; ++j)
            var.p[j] += mat[i][j] + 1;

    int check = 0;
    for (int i = 0; i < var.N; ++i)
        check += var.p[i];

    assert(check = 200*199/2 + 200);
}

