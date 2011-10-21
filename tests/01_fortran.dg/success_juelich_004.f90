! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
!
!    The following program is adopted from the IBM Optimization and Tuning Guide
! for Fortran, C, and C++ (SC09-1705-01) that is designed to determine the
! maximum MFLOP rate on a machine.
! - This version should give results within 1% of the
!   theoretical Mflop rate on several (otherwise idle) machines. 
! - It should be compiled with -O
! - The assignments are ordered in a way that avoids delays on several processors
!   (Sometimes additional floating point units on several processors can be
!    slowed down if dependent operations are placed too close together)
! - There are 800 million floating point operations in this program
! - The values in the assignments have been choosen so as not to underflow
!   or overflow.
!   Exceptional conditions slow down programs and keep them from reaching
!   their theoretical maximums.
!
! The printed result for FP1 should be: 47026.662436625607
!     
! K.Wolkersdorfer@fz-juelich.de (16.1.2001)
!
program theory
       implicit none
       REAL*8 FP1, FP2, FP3, FP4, FP5, FP6, FP7, FP8, FP9
       REAL*8 FJ1, FJ2, FJ3, FJ4, FJ5, FJ6, FJ7, FJ8, FJ9
       REAL*8 SEC, RATE, tstart, tend
       integer i, ic1, im1, ic2, im2, ir1, ir2
       FP1=0.00000100D0
       FP2=0.00000001D0
       FP3=0.00000002D0
       FP4=0.00000003D0
       FP5=0.00000004D0
       FP6=0.00000001D0
       FP7=0.00000011D0
       FP8=0.00000017D0
       FP9=0.00000041D0
       FJ1=FP1
       FJ2=FP2
       FJ3=FP3
       FJ4=FP4
       FJ5=FP5
       FJ6=FP6
       FJ7=FP7
       FJ8=FP8
       FJ9=FP9
       CALL SYSTEM_CLOCK(COUNT=IC1, COUNT_RATE=IR1, COUNT_MAX=IM1)
       DO I=1,50000000
          FP4=FP4*FP9+FP6
          FJ4=FJ4*FJ9+FJ6
          FP6=FP6*FP9+FP8
          FJ6=FJ6*FJ9+FJ8
          FP8=FP8*FP9+FP2
          FJ8=FJ8*FJ9+FJ2
          FP3=FP3*FP9-FP5
          FJ3=FJ3*FJ9-FJ5
          FP5=FP5*FP9-FP7
          FJ5=FJ5*FJ9-FJ7
          FP7=FP7*FP9-FP2
          FJ7=FJ7*FJ9-FJ2
          FP2=FP2*FP9+FP4
          FJ2=FJ2*FJ9+FJ4
          FP1=FP1*FP9-FP3
          FJ1=FJ1*FJ9-FJ3
       ENDDO

   CALL SYSTEM_CLOCK(COUNT=IC2, COUNT_RATE=IR2, COUNT_MAX=IM2)
!      tend = date_time(8)
       
WRITE (6,*) "Result:", FP1
  WRITE (6,*) "1st CALL SYSTEM_CLOCK:", IC1,IR1,IM1
  WRITE (6,*) "2nd CALL SYSTEM_CLOCK:", IC2,IR2,IM2
  SEC  = IR2
  SEC  = (IC2-IC1)/SEC
!       sec = t2(1)-t1(1)
! 	sec = tend-tstart
RATE = 1600./SEC
WRITE (6,*) "Number of (wall) clock seconds:", SEC
write (6,*) "CPU time measured ",tend-tstart
WRITE (6,*) "MFlop rate = ", RATE
end
