! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>

subroutine fun_task(x, y)
    implicit none
   integer :: x, y(10)

   x = x+ 1
   y(1) = y(1) + 1
end subroutine

program main
    implicit none
    interface

    !$omp task inout(x, y)
    subroutine fun_task(x, y)
       integer :: x
       integer :: y(10)
    end subroutine

    end interface

   integer :: i, j(10)

   i = 41
   j = 10041

   call fun_task(i, j)
   !$OMP TASKWAIT

   IF (i /= 42) THEN
       STOP 1
   END IF

   IF (j(1) /= 10042) THEN
       STOP 2
   END IF
end program
