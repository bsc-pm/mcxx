/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
void f(void)
{
   _Complex float cf;
   _Complex double cd;
   _Complex long double cld;
   double d;
   float f;
   long double ld;

   cf = 1.0;
   d = cf;
   f = cf;
   ld = cf;

   cd = 1.0;
   d = cd;
   f = cd;
   ld = cd;

   cld = 1.0;
   d = cld;
   f = cld;
   ld = cld;
}
