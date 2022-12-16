/*
<testinfo>
test_generator="config/mercurium-ompss-2"
test_ignore=yes
test_ignore_reason="task for is not supported anymore"
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

    #pragma oss task for chunksize(4)
    for (int i = 0; i < 100; ++i)
    {}

    #pragma oss taskwait
}
