/*
<testinfo>
test_generator="config/mercurium-cxx14"
</testinfo>
*/
double operator "" _h1(unsigned long long int secs)
{
    return secs / 60.0;
}

int main() {
    double var1 = 90_h1;
    double var2 = 0x90_h1;
    double var3 = 0b100_h1;
}
