/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/
int foo() {
    int* v[10];
    for (int i = 0;  i < 10; ++i) v[i] = new int;
    for (auto it : v)
        delete it;
}
