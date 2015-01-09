/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

int main(int, char**)
{
    [](int x) { return x + 1; }(2);

    int y, z;
    int (*increment)(int) = [](int x) { return x + 2; };
}
