/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/
void f(const int &a);
void f(const int &&a);
int main() {
    f({1});
}
