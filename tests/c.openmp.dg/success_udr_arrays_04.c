#include <string.h>

void add_vec(float*, float*, int);

void add_mat(float*, float*, int, int);

#pragma omp declare reduction(add_vec : float) dimensions(1)

#pragma omp declare reduction(add_mat : float) dimensions(2)

#define VECTOR_SIZE (100)
#define NUM_VECTORS (100)

void f1(void)
{
    float array_of_vec[NUM_VECTORS][VECTOR_SIZE];
    float vec_sum[VECTOR_SIZE];

    memset(vec_sum, 0, sizeof(vec_sum));
    
    int i; 
#pragma omp parallel for reduction(add_vec : vec_sum)
    for (i = 0; i < NUM_VECTORS; i++)
    {
        add_vec(&vec_sum[0], &array_of_vec[i][0], VECTOR_SIZE);
    }
}

#define NUM_MATS (100)

void f2(void)
{
    float array_of_mat[NUM_MATS][VECTOR_SIZE][VECTOR_SIZE];
    float mat_sum[VECTOR_SIZE][VECTOR_SIZE];

    memset(mat_sum, 0, sizeof(mat_sum));
    
    int i; 
#pragma omp parallel for reduction(add_mat : mat_sum)
    for (i = 0; i < NUM_MATS; i++)
    {
        add_mat(&mat_sum[0][0], &array_of_mat[i][0][0], VECTOR_SIZE, VECTOR_SIZE);
    }
}
