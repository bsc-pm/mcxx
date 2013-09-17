! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
module p
        implicit none
        contains
        subroutine foo
        integer j

        set_loop: do j=1,10
        end do set_loop

        contains
                subroutine var
                integer i
                set_loop: do i=1,10
                end do set_loop
                end subroutine
        end subroutine
end module
