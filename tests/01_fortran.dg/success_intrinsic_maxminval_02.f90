! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program maxv

        type  :: cell
           real , dimension(1000)     ::  vcm
           integer                    ::  n_slv
        end type cell

        type (cell), dimension(:,:), allocatable, save::  cel

        integer                       :: v

        allocate (cel(2,2))

        do i = 1,1000
           cel(1,1)%vcm(i) = real(i)
        enddo

        v = maxval(cel%vcm(3))
        print *, v
        v = minval(cel%vcm(3))
        print *, v

end program maxv

