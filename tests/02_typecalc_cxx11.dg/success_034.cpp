/*
<testinfo>
test_generator="config/mercurium-cxx11 run"
</testinfo>
*/

struct A
{
    unsigned long x1 = sizeof(A);
    unsigned long x2 = sizeof(struct A);
    unsigned long x3 = sizeof(*this);
};

int main(int argc, char *argv[])
{
    A a;

    return !(a.x1 == (3 * sizeof(unsigned long))
            && a.x2 == a.x1
            && a.x3 == a.x1);
}
