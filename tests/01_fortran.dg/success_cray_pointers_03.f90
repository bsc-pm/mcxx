! <testinfo>
! test_generator="config/mercurium-fortran"
! compile_versions="cray"
! test_FFLAGS_cray="-fcray-pointer"
! </testinfo>
SUBROUTINE sub(size)
    IMPLICIT NONE
    INTEGER :: SIZE

    POINTER (Pbuffer,MPI_Buffer(size))
    REAL(kind=8) :: MPI_Buffer

    PRINT *, Pbuffer, MPI_Buffer
END SUBROUTINE sub
