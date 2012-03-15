! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program p

        integer, parameter :: integer_8 = selected_int_kind(18)

        type :: memory
           integer (kind = integer_8)   :: slt_size
           integer (kind = integer_8)   :: slv_size
           integer (kind = integer_8)   :: total
        end type memory

        type(memory)                    :: mem

        print *, 'Hello', mem
end program p

