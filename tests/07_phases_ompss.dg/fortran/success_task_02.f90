! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
program task

   integer N
   parameter (N=10)
   integer BS
   parameter (BS=5)

   double precision X(N)
   common // X

   integer, external :: omp_get_thread_num

do i = 1, N, BS
!$omp task out (X(i:i+BS-1)) ! each task has( a range of the vector as out
    print *, 'task 1', i, i+BS-1, ' on thread ', omp_get_thread_num()
    do j = i, i+BS-1
       X(j) = dble(j)
    enddo
    print *, 'task 1 ends', i, i+BS-1, ' on thread ', omp_get_thread_num()
!$omp end task
enddo

! this task can only start when the data in the last range is available
!$omp task priority(1) in (X(N-BS+1:N))
    print *, 'task 2', N-BS+1, N, ' on thread ', omp_get_thread_num()
    do j = N-BS+1,N
       if ((abs (X(j) - dble(j))) .gt. 0.0001) THEN
           print *, 'Error in task 2', j, X(j)
           STOP 2
       END IF
    enddo
!$omp end task

!!$omp taskwait
!$omp task priority(1) in (X(N-BS+1:N)) !! )
    print *, 'task 3'
    do j = N-BS+1,N
       if ((abs (X(j) - dble(j))) .gt. 0.0001) THEN
           print *, 'Error in task 3', j, X(j)
           STOP 3
       END IF
    enddo
!$omp end task

    print *, 'Waiting...'
!$omp taskwait

    print *, 'The end.'

end program task
