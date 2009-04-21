void f(void)
{
    int a;

#pragma hlt fusion
    {
        a = 3;
    }
}
