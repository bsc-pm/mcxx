struct A
{
    char num_d;
    int d[];
};


int main(int argc, char *argv[])
{
    int i = sizeof(struct A);
    return 0;
}
