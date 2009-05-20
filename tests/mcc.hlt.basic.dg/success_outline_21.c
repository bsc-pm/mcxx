int a;

void f(int b)
{
    int c;

#pragma hlt outline packed
    {
        a = 3;
        b = 4;
        c = 5;
    }
}
