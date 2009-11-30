#include <string.h>

void add_vec(int, float*, float*);

void add_mat(int, int, float*, float*);

#pragma omp declare reduction(add_vec : float) dimensions(1)

#pragma omp declare reduction(add_mat : float) dimensions(2)

#define VECTOR_SIZE (100)
#define NUM_VECTORS (100)

void f(void)
{
    float array_of_vec[NUM_VECTORS][VECTOR_SIZE];
    float total_sum[VECTOR_SIZE];

    memset(total_sum, 0, sizeof(total_sum));
    
    int i; 
#pragma omp parallel for reduction(add_vec : total_sum)
    for (i = 0; i < NUM_VECTORS; i++)
    {
        add_vec(VECTOR_SIZE, total_sum, array_of_vec[i]);
    }
}
