/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
void f()
{
    static_assert(0b111 == 7, "Invalid binary literal");
    static_assert(0b111L == 7L, "Invalid binary literal");
    static_assert(0b111U == 7U, "Invalid binary literal");
}
