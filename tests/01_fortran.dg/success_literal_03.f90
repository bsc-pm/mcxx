! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program real

        integer, parameter :: real_8 = selected_real_kind(13)

        real (kind=real_8) :: seed

        seed = 16807.0_real_8 / 10e2

        print *, 'Hello, this is the result', seed
end program real

