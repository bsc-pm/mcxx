/*
<testinfo>
test_generator=config/mercurium-nanos4
test_ignore=yes
</testinfo>
*/
#include <string.h>

void add_vec(int, float*, float*);

void add_mat(int, int, float*, float*);

#pragma omp declare reduction(add_vec : float) dimensions(1)

#pragma omp declare reduction(add_mat : float) dimensions(2)

void f1(int num_vectors, int vector_size)
{
    float array_of_vec[num_vectors][vector_size];
    float vec_sum[vector_size];

    memset(vec_sum, 0, sizeof(vec_sum));
    
    int i; 
#pragma omp parallel for reduction(add_vec : vec_sum)
    for (i = 0; i < num_vectors; i++)
    {
        add_vec(vector_size, &vec_sum[0], &array_of_vec[i][0]);
    }
}

void f2(int num_mats, int vector_size)
{
    float array_of_mat[num_mats][vector_size][vector_size];
    float mat_sum[vector_size][vector_size];

    memset(mat_sum, 0, sizeof(mat_sum));
    
    int i; 
#pragma omp parallel for reduction(add_mat : mat_sum)
    for (i = 0; i < num_mats; i++)
    {
        add_mat(vector_size, vector_size, &mat_sum[0][0], &array_of_mat[i][0][0]);
    }
}
