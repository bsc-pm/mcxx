/*
<testinfo>
test_generator=config/mercurium-ompss
</testinfo>
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#include <sys/times.h>
#include <unistd.h>
#include <errno.h>

#pragma omp task inout( (A)[0;NB*NB]) //priority(1)
void potrf_tile(int NB, double (*A)[NB*NB])
{
}

int main(int argc, char* argv[]){
    int ts = 2;
    int nt = 8;
    double ** Ah;

    Ah = (double **) malloc(nt * nt * sizeof(float *));

    potrf_tile(ts, Ah[4]);

    return 0;
}


