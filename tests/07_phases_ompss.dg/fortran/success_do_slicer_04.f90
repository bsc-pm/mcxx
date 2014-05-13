! <testinfo>
! #test_generator=config/mercurium-ompss
! test_ignore=yes
! test_ignore_reason="omp_get_max_threads does not work in OmpSs"
! </testinfo>
PROGRAM MAIN
  IMPLICIT NONE
  INTEGER :: k, j
  INTEGER, EXTERNAL :: OMP_GET_THREAD_NUM, OMP_GET_MAX_THREADS

  INTEGER :: M
  INTEGER :: TH_S, TH_P

  M = OMP_GET_MAX_THREADS()
  TH_S = -1
  TH_P = -1

  PRINT *, "START..."

  do 1040 k  = 1, 2
     TH_S = MAX(TH_S, OMP_GET_THREAD_NUM())
     !$OMP PARALLEL DO FIRSTPRIVATE(M) SCHEDULE(STATIC)
     do 1040 j  = 1, 10*M
        !$OMP CRITICAL
        TH_P = MAX(TH_P, OMP_GET_THREAD_NUM())
        !$OMP END CRITICAL
  1040    continue

  PRINT *, "DONE"

  IF (TH_S /= 0) STOP 1
  IF (TH_P /= M-1) STOP 2

END PROGRAM MAIN
