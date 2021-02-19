/*
<testinfo>
test_generator=config/mercurium-ompss-2
test_compile_fail=yes
</testinfo>
*/

#define L1C 32768

typedef long   intT  ;
typedef double floatT;

#define MIN(X1, X2) ((X1) < (X2) ? (X1) : (X2))

#define MAX(X1, X2) ((X1) > (X2) ? (X1) : (X2))

static const intT l1c = L1C, iZero = 0, iOne = 1;

template<class objT>
void getvm_task(const intT layout, const intT   trans                , const intT m   , const intT n        ,
                const objT alpha , const objT * __restrict__ const a ,
                const intT lda   , const objT * __restrict__ const x , const intT incx,
                                   const objT * __restrict__ const y0, // Dummy argument
                const objT beta  ,       objT * __restrict__ const y , const intT incy, const intT nb = iOne)
{
  static const objT objTsize = sizeof(objT)                ;
         const intT touchMem = (m*n + m + n)*objTsize      ,
                    nbA      = (touchMem < l1c) ? iOne : nb;

  const intT bsN   = n/nbA,
             zsize = m*lda;

  for(intT j = iZero; j < n; j += bsN)
  {
    const intT bsNA = MIN(bsN, n - j),
               jl   = j / lda        ,
               jrem = j % lda        ;

    const intT jc    = (j + MIN(bsNA, lda))/lda                    ,
               jr    = (j +     bsNA      )/lda                    ,
               lsize = MIN(j + lda - jrem, j + bsNA) - j           ,
               rsize = MAX(j + bsNA - (jl + iOne)*lda, iZero) % lda, // (jr == jl) ? iZero : (j + bsNA) % lda;
               r1    = MAX(rsize, iOne)                            ;

    const objT * __restrict__ const p_al = a + zsize*jl;
    const objT * __restrict__ const p_ac = a + zsize*jc;
    const objT * __restrict__ const p_ar = a + zsize*jr;

    #pragma oss task default(none) firstprivate(layout, trans, alpha, beta, incx, incy)         \
                                   firstprivate(j, jrem, m, bsNA, lda) firstprivate(x, p_al, y) \
                                   in (         x             [iZero;m    ])                    \
                                   in (([m][lda]p_al)[iZero;m][jrem ;lsize])                    \
                                   out(         y             [j    ;bsNA ])
    {
      // dummy function
      // getvm(layout, trans, m, bsNA, alpha, p_al + jrem, lda, x, incx, y, beta, y + j, incy);
    }
  }
}

