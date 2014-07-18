/*
<testinfo>
test_generator="config/mercurium-cxx11 run"
</testinfo>
*/
#include <stdlib.h>
#include <string.h>

const char* c1 = R"FOO(one
two
three)FOO";
const char* c1_test = "one\ntwo\nthree";

const char* c2 = R"FOO(one\n
two\n
three)FOO";
const char* c2_test = "one\\n\ntwo\\n\nthree";

int main(int argc, char *argv[])
{
    if (strcmp(c1, c1_test) != 0)
        abort();

    if (strcmp(c2, c2_test) != 0)
        abort();

    return 0;
}
