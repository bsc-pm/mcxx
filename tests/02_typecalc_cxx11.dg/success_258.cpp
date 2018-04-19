/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/
struct Data {
    const int& get_ref();
};

int main() {

    Data  d[100];
    for (auto a : d)
    {
        const auto &p = a.get_ref();
    }
}
