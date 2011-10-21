! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program test
implicit none
integer :: d1, d2, d3
integer :: di(3)

do d2 = 1,3
   do d1 = d2+1,3
      do d3=3,1,-1
         if(d3.ne.d1 .and. d3.ne.d2) exit
      enddo

      di = [d1,d2,d3]
      write(*,*) d1, d2, d3, di

   enddo
enddo

write(*,*) di([1,2])
write(*,*) di([1,3])
write(*,*) di([2,3])

end program test
