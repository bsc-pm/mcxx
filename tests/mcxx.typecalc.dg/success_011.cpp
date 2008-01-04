float f(char, long);
float (*v[2])(char, long) = { 0, f };

void k()
{
    (v[1])(3, 4);
}
