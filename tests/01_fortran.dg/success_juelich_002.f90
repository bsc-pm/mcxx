! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program t
implicit none
integer :: tmp(3), ids(2), c
tmp(1) = 1
tmp(2) = 1
tmp(3) = 4

write(*,*) tmp(minloc(tmp))

ids(1)=1
ids(2)=3
write(*,*) ids(minloc(tmp([ids])))
write(*,*) ids(maxloc(tmp([ids])))
ids(1)=3
ids(2)=3
write(*,*) ids(minloc(tmp([ids])))
write(*,*) ids(maxloc(tmp([ids])))

write(*,*) tmp
write(*,*) product(tmp)
c=1
do while (c.lt.100)
   write(*,*) c
   c=c*2
end do
end program t
