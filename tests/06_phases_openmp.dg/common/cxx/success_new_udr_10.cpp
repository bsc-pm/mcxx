/*
<testinfo>
test_generator=config/mercurium-omp

test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes
</testinfo>
*/

typedef struct {
  int x ;
  int y ;
} point_t;

int max(int x, int y ) 
{
    ( x ) > ( y ) ? ( x ) : ( y ) ;
}

#pragma omp declare reduction( maxarea : point_t : \
_out.x = max(_out.x , _in.x ), _out.y = max(_out.y , _in.y ) ) \
identity( {0 ,0} )

int main (int argc, char* argv[])
{
   point_t pt;

   #pragma omp parallel reduction (maxarea : pt)
   pt;

   return 0;
}
