! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM A

   IMPLICIT NONE

   TYPE rt_type
     INTEGER :: mark1
   END TYPE rt_type

   TYPE(rt_type), PARAMETER :: rt_init = rt_type(3)
   TYPE(rt_type) :: rt(10)

   rt(:) = rt_type(0)

   !$OMP PARALLEL SHARED(RT) DEFAULT(NONE)
   !$OMP SINGLE
   rt(:) = rt_init
   !$OMP END SINGLE
   !$OMP END PARALLEL

   if (ANY(rt(:)%mark1 /= 3)) STOP 1

END PROGRAM A
