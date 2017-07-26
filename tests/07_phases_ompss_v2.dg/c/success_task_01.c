/*
<testinfo>
test_generator="config/mercurium-ompss-v2"
test_nolink=yes
</testinfo>
*/

#pragma oss task
void f()
{ }

void g() {
    #pragma oss task
    {
        f();
    }

    #pragma oss task loop chunksize(4)
    for (int i = 0; i < 100; ++i)
    {}

    #pragma oss taskwait
}
