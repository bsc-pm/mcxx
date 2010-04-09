/*
<testinfo>
test_generator=config/mercurium-nanox
</testinfo>
*/

#define N 10

void hi (int i);

int main (int argc, char *argv[])
{
  int i;

  for (i = 0; i < N; i++) {
#pragma omp target device(smp_numa) copy_deps
#pragma omp task
    hi(i);
  }
}

