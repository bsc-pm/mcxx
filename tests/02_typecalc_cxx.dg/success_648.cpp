/*
<testinfo>
test_generator="config/mercurium run"
</testinfo>
*/


struct C
{
    static const int x = 1; // This behaves as a declaration
};

const int C::x; // This is the definition!

const int *p = &C::x;
int main() {}

