/*
<testinfo>
test_generator=config/mercurium-parallel-simd
</testinfo>
*/

typedef
struct Point
{
    float *coord;
} Point;

float dist(Point p1, Point p2, int dim) {

  int i;
  float result=0.0f;

#pragma omp simd for reduction(+:result) //suitable(dim) //simdlen(dim)
  for (i=0;i<dim;i++)
  {
    result += (p1.coord[i] - p2.coord[i])*(p1.coord[i] - p2.coord[i]);
  }

  return(result);
}

