! <testinfo>
! test_generator="config/mercurium-ompss-2"
! test_ENV=NANOS6_SCHEDULER=naive
! </testinfo>

program p
    implicit none
    integer, parameter :: num_iterations = 1000
    real(8), parameter :: q = 1.0
    real(8), parameter :: epsilon = 0.00025
    integer :: i
    real(8) :: validate
    real(8) :: res = 0

    !$oss loop shared(res)
    do i = 1, num_iterations
        !$oss atomic
        res = res + q*i
    end do

    !$oss taskwait

    validate = (num_iterations + 1)*num_iterations/2

    if (res <= validate - epsilon .or. res >= validate + epsilon) then
        print *, "Error, res != validate"
        stop -1
    end if
end program

