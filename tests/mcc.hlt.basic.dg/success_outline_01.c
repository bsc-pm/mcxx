int a;

void f(int b)
{
    int c;

#pragma hlt outline
    {
        a = 3;
        b = 4;
        c = 5;
    }
}
