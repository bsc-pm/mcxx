! <testinfo>
! test_generator=config/mercurium-ompss-2
! test_nolink=yes
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: V(:)
    INTEGER, PARAMETER ::  N = 100

    !$OSS LINT ALLOC(V(1:N))
        ALLOCATE(V(N))
    !$OSS END LINT

    !$OSS LINT FREE(V)
        DEALLOCATE(V)
    !$OSS END LINT
END PROGRAM P
