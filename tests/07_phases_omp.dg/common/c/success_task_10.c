/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/

void f(int x, int y)
{
#pragma omp task
    {
        struct A
        {
            int x;
            int y;
        };

        struct A a;
        a.x = x;
        a.y = y;
    }
}

int main(int argc, char* argv[])
{
    f(3, 4);

    return 0;
}
