#include <cstring>
#include <cstdlib>

#define N 3
#define M 5
#define ROW 128
#define COL 128

int main(int argc, char** argv)
{
    // *** Static allocation *** //
    int *array1 = NULL;
    array1 = new int[10];
    array1[1] = 0;
    
    int array2[5] = { 1, 2, 3, 4, 5 };
    
    int array3[] = { 6, 7, 8, 9, 10 };
    
    int array4 [N][M];
    for (int n=0 ; n<N ; n++)
        for (int m=0; m<M; m++)
            array4[n][m] = (n+1)*(m+1);
        
    int *ptr = new int;
    
    // *** Dynamic allocation *** //
    int (*array6)[COL];
    array6 = (int(*)[COL]) malloc(sizeof(int) * ROW * COL);
    
    // *** Free memory *** //
    delete[] array1;
    array1 = NULL;
    
    delete ptr;
    
    free(array6);
    
    return 0;
}