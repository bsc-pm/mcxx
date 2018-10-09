/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests omp for data-sharings*/

#include <assert.h>

int main(int argc, char *argv[]) {
    int s_a;
    int s_b[10];
    int s_c[argc];

    int p_a;
    int p_b[10];
    int p_c[argc];

    int fp_a;
    int fp_b[10];
    int fp_c[argc];

    int l_a;
    int l_b[10];
    int l_c[argc];

    int flp_a;
    int flp_b[10];
    int flp_c[argc];

    #pragma omp parallel
    #pragma omp for shared(s_a, s_b, s_c) \
                    private(p_a, p_b, p_c) \
                    firstprivate(fp_a, fp_b, fp_c, flp_a, flp_b, flp_c) \
                    lastprivate(l_a, l_b, l_c, flp_a, flp_b, flp_c)
    for (int i = 0; i < 10; ++i) {
        s_a = s_b[0] = s_c[0];
        p_a = p_b[0] = p_c[0];
        fp_a = fp_b[0] = fp_c[0];
        l_a = l_b[0] = l_c[0];
        flp_a = flp_b[0] = flp_c[0];
    }
}
