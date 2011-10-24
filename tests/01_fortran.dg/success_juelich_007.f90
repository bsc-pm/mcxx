! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program sm
implicit none

real*8 :: vec(3), res, const, squ(3), co
integer :: c, iter

integer :: strt, stp, tcks

! init values
call random_number(vec)
call random_number(const)
co = const

const=co
!write(*,*) 'start 1'
call system_clock(strt)
do iter=1,200000000
   const=const+iter
   res = 1.d0 / ( const + sum(vec**2) )
   call dummy(res)
end do
call system_clock(stp,tcks)
!write(*,'(a,f8.4,a)') 'finish 1, took ',real(stp-strt)/tcks,' secs'
write(*,'(f8.4,1x)',advance='no') real(stp-strt)/tcks

const=co
!write(*,*) 'start 2'
call system_clock(strt)
do iter=1,200000000
   const=const+iter
   res = 1.d0 / ( const + vec(1)*vec(1)+vec(2)*vec(2)+vec(3)*vec(3) )
   call dummy(res)
end do
call system_clock(stp,tcks)
!write(*,'(a,f8.4,a)') 'finish 2, took ',real(stp-strt)/tcks,' secs'
write(*,'(f8.4,1x)',advance='no') real(stp-strt)/tcks

const=co
!write(*,*) 'start 3'
call system_clock(strt)
do iter=1,200000000
   const=const+iter
   res = const
   do c=1,3
      res = res + vec(c)**2
   end do
   res = 1.d0 / ( res )
   call dummy(res)
end do
call system_clock(stp,tcks)
!write(*,'(a,f8.4,a)') 'finish 3, took ',real(stp-strt)/tcks,' secs'
write(*,'(f8.4,1x)',advance='no') real(stp-strt)/tcks

const=co
!write(*,*) 'start 4'
call system_clock(strt)
do iter=1,200000000
   const=const+iter
   res = const + vec(1)**2
   res = res + vec(2)**2
   res = res + vec(3)**2
   res = 1.d0 / ( res )
   call dummy(res)
end do
call system_clock(stp,tcks)
!write(*,'(a,f8.4,a)') 'finish 4, took ',real(stp-strt)/tcks,' secs'
write(*,'(f8.4,1x)',advance='no') real(stp-strt)/tcks

const=co
!write(*,*) 'start 5'
call system_clock(strt)
do iter=1,200000000
   const=const+iter
   res = const + vec(1)**2 + vec(2)**2 + vec(3)**2
   res = 1.d0 / ( res )
   call dummy(res)
end do
call system_clock(stp,tcks)
!write(*,'(a,f8.4,a)') 'finish 5, took ',real(stp-strt)/tcks,' secs'
write(*,'(f8.4,1x)',advance='no') real(stp-strt)/tcks

const=co
!write(*,*) 'start 6'
call system_clock(strt)
do iter=1,200000000
   const=const+iter
   squ = vec**2 
   res = const + sum(squ)
   res = 1.d0 / ( res )
   call dummy(res)
end do
call system_clock(stp,tcks)
!write(*,'(a,f8.4,a)') 'finish 6, took ',real(stp-strt)/tcks,' secs'
write(*,'(f8.4,1x)',advance='no') real(stp-strt)/tcks

const=co
!write(*,*) 'start 7'
call system_clock(strt)
do iter=1,200000000
   const=const+iter
   squ = vec**2 
   res = const + squ(1)+squ(2)+squ(3)
   res = 1.d0 / ( res )
   call dummy(res)
end do
call system_clock(stp,tcks)
!write(*,'(a,f8.4,a)') 'finish 7, took ',real(stp-strt)/tcks,' secs'
write(*,'(f8.4,1x)',advance='no') real(stp-strt)/tcks

const=co
!write(*,*) 'start 8'
call system_clock(strt)
do iter=1,200000000
   const=const+iter
   do c=1,3
      squ(c) = vec(c)*vec(c)
   end do
   res = const + squ(1)+squ(2)+squ(3)
   res = 1.d0 / ( res )
   !if(iter.eq.-1) write(*,*) res
   call dummy(res)
end do
call system_clock(stp,tcks)
!write(*,'(a,f8.4,a)') 'finish 8, took ',real(stp-strt)/tcks,' secs'
write(*,'(f8.4,1x)',advance='no') real(stp-strt)/tcks

write(*,*) ' '

contains

subroutine dummy(invar)

implicit none

real*8, intent(inout) :: invar
real*8 :: var = 0.d0

var = var + invar

return

end subroutine dummy

end program sm
