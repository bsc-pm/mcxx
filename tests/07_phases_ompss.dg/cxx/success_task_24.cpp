/*
<testinfo>
test_generator=config/mercurium-ompss
test_CXXFLAGS="--no-copy-deps"
</testinfo>
*/

struct A
{
  int g()
  {
    #pragma omp task
    {
    }
    #pragma omp taskwait
  }

  int f()
  {
    #pragma omp task
    {
      #pragma omp critical
      {
      }
    }
    #pragma omp taskwait
  }
};

int main(int argc, char *argv[])
{
    A a;
    a.g();
    a.f();

    return 0;
}
