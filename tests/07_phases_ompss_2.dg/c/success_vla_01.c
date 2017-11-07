/*
<testinfo>
test_generator="config/mercurium-ompss-2"
</testinfo>
*/
#include<assert.h>

int main(int argc, char*argv[])
{
    int v[10];
    int *p1_v = v;
    int (*p2_v)[10]=&v;

    int vla[argc];
    int *p1_vla = vla;
    int (*p2_vla)[argc]=&vla;


    int vla2[argc][argc];
    int (*p1_vla2)[argc] = vla2;
    int (*p2_vla2)[argc][argc] = &vla2;

    #pragma oss task shared(v, p1_v, p2_v) shared(vla, p1_vla, p2_vla) shared(vla2, p1_vla2, p2_vla2)
    {
        v[0]       = 0;
        vla[0]     = 0;
        vla2[0][0] = 0;

        v[0]++;
        (*p1_v)++;
        (*p2_v)[0]++;

        vla[0]++;
        (*p1_vla)++;
        (*p2_vla)[0]++;

        vla2[0][0]++;
        (*p1_vla2)[0]++;
        (*p2_vla2)[0][0]++;
    }

#pragma oss taskwait
    assert(v[0]        == 3);
    assert(vla[0]      == 3);
    assert(vla2[0][0]  == 3);

}
