! <testinfo>
! test_generator=config/mercurium-omp
! compile_versions="generate_mod use_mod"
! test_FFLAGS_generate_mod="-DMOD1 -DMOD2 -DEMPTY_MAIN"
! test_FFLAGS_use_mod="-DMOD1 -DTEST_MAIN"
! test_nolink=yes
! </testinfo>

#ifdef MOD1
module element_mod

  type, public :: index_t
     integer :: NumUniquePts
  end type index_t

  type, public :: element_t
     type (index_t) :: idxP
  end type element_t

end module element_mod
#endif

#ifdef MOD2
module dyn_grid2
  use element_mod, only : element_t
  type(element_t), dimension(:), pointer :: elem
end module dyn_grid2
#endif

#ifdef TEST_MAIN
program main

    use element_mod, only : element_t
    use dyn_grid2,       only : elem

    implicit none
    integer :: i, n

    n = 10

    allocate(elem(n))

    do i=1,n
        elem(i)%idxp%NumUniquePts = i
    end do

    !$omp parallel do
    do i=1,n
        elem(i)%idxp%NumUniquePts = elem(i)%idxp%NumUniquePts + 1
    end do

    do i=1,n
        IF (elem(i)%idxp%NumUniquePts /= (i + 1)) THEN
            PRINT *, "WRONG ELEMENT", i
            STOP 1
        END IF
    end do

    deallocate(elem)

end program main
#endif

#ifdef EMPTY_MAIN
program main
    continue
end program main
#endif
