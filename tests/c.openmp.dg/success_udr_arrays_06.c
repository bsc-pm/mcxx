/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/
#include <string.h>

void add_vec(int, float*, float*);

void add_mat(int, int, float*, float*);

#pragma omp declare reduction(add_mat : float) dimensions(2)

#define VECTOR_SIZE (100)
#define NUM_MATS (100)

void f2(float p_array_of_mat[NUM_MATS][VECTOR_SIZE][VECTOR_SIZE], 
        float p_mat_sum[VECTOR_SIZE][VECTOR_SIZE])
{
    float (*mat_sum)[NUM_MATS][VECTOR_SIZE] = 
        (float (*)[NUM_MATS][VECTOR_SIZE])p_mat_sum;

    memset((*mat_sum), 0, sizeof((*mat_sum)));
    
    int i; 
#pragma omp parallel for reduction(add_mat : mat_sum)
    for (i = 0; i < NUM_MATS; i++)
    {
        add_mat(VECTOR_SIZE, VECTOR_SIZE, &(*mat_sum)[0][0], &p_array_of_mat[i][0][0]);
    }
}
