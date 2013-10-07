/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

int ieee754_isfinite(double __x) throw ()
{
  return (__extension__
   (((((union { double __d; int __i[2]; }) {__d: __x}).__i[1]
      | 0x800fffffu) + 1) >> 31));
}

