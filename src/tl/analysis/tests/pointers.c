int main(int argc, char ** argv)
{
  int a = 5, b = 6;
  int * pa = &a;
  int * pb = &b;

  int * c;

  // Bitwise operators
  c = ~pa;
  c = pa & pb;
  c = pa | pb;
  c = pa ^ pb;
  c = pa << pb;
  c = pa >> pb;

  // Assignment operators
  c += pa;
  c %= pa;
  c &= pa;
  c >>= pa;

  // Arithmetic operators
  c = pa;
  pa + pb = 5;


  return 0;
}
