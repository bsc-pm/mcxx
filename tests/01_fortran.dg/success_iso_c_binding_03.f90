! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program p
        use iso_c_binding
        type(c_ptr)::ps
        interface
                subroutine s(ps)
                        use iso_c_binding
                        type(c_ptr),value::ps
                end subroutine
        end interface

        call s(ps)
end program p
