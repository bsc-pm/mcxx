/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/

int main(int argc, char* argv[])
{
    int i;
#pragma omp for default(none)
    for(i=0;i<8;i++)
    {
    }

#pragma omp parallel for default(none)
    for(i=0;i<8;i++)
    {
    }

    return 0;
}
