struct stat
{
    int a;
};

void stat(int n);

void f()
{
    int a;

    stat(a);
}
