! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>

program main
    implicit none
    interface
        !$omp task inout(a)
        subroutine sub(a)
                implicit none
                logical :: a
        end subroutine sub
    end interface

    logical :: b
    b = .false.
    call sub(b)

    !$OMP TASKWAIT

    IF (.NOT. b) STOP 1
end program main

subroutine sub(a)
        implicit none
        logical :: a

        a = .true.
end subroutine sub
