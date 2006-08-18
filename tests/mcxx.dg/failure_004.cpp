// namespace A
// {
//     int f(int n)
//     {
//         return n+1;
//     }
// };

void g()
{
    int k;
    using A::f;
    k = f(3);
}
