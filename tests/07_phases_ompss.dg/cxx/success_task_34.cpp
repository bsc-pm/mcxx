/*
<testinfo>
test_generator=("config/mercurium-ompss-2 openmp-compatibility")
test_ignore="yes"
test_ignore_reason="nanos6 does not have support for this yet"
</testinfo>
*/


int main(int argc, char *argv[])
{
    char *auto_ = NULL;
    
    #pragma omp task auto(auto_[1;-2])
    {
    }


    #pragma omp taskwait

    return 0;
}
