/*
<testinfo>
test_generator=config/mercurium-ompss
test_compile_fail_nanos6_mercurium=yes
test_compile_fail_nanos6_imcc=yes
</testinfo>
*/
void dealloc_tiled_matrix(int MBS, int NBS, int M, int N, double (*a)[N/NBS][MBS][NBS]) {
        // Build a fictitious dependency structure to free the whole tiled matrix at once
        for (int i=0; i<M/MBS; i ++) {
                for (int j=0; j<N/NBS; j++) {
                        if (i != 0 || j != 0) {
                                #pragma omp task inout(a[i][j]) concurrent(a[0][0])
                                {
                                }
                        }
                }
        }

        //#pragma omp task inout(a[0][0])
        //free(a);
}


int main(int argc, char *argv[])
{
    return 0;
}
