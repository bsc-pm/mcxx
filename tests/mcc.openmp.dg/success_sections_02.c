 int
main (int argc, char *argv[])
{
  int a, b;

#pragma omp parallel
#pragma omp sections
{
#pragma omp section
   a = 0;
#pragma omp section
   b = 0;
}

   return 0;
}
