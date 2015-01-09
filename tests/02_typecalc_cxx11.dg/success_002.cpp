/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

enum A1 : unsigned long {
    V1 = 1
};

enum A2 {
    V2 = 1
};

unsigned long* foo(unsigned long);
int* foo(int);
// unsigned int* foo(unsigned int);

void g()
{
    unsigned long *pul1 = 0;
    pul1 = foo(V1);

    int *pul2 = 0;
    pul2 = foo(V2);
}
