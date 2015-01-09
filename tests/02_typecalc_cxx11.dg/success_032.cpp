/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

void g1(int *k);
void g2(const int * k);

void g3(int *&k);
void g4(int *const &k);

void f(void)
{
    auto ptr = nullptr;
    g1(nullptr);
    g2(nullptr);
    // g3(nullptr); // ERROR. Cannot bind std::nullptr_t to int*&
    g4(nullptr);

    g1(ptr);
    g2(ptr);
    // g3(ptr); // ERROR. Cannot bind std::nullptr_t& to int*&
    g4(ptr);
}
