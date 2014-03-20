/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template<class M>
class matrix_row
{
    inline void swap(matrix_row m)
    {
    }
};

void g()
{
    matrix_row<int> a;
    matrix_row<float> b;
    matrix_row<double> c;
}
