! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program f_char_test

    character(len=8)    ::  test_str 
    character(len=8)    ::  test_str_array(10) 
    type                ::  combined_str
        character(len=8)     ::  test_str
        character(len=8)     ::  test_str_array(3)
    end type combined_str

    CHARACTER (LEN=10), ALLOCATABLE :: P(:)

    type(combined_str)  ::  test_struct

    test_struct%test_str = 'abcdefgh'
    test_struct%test_str_array = (/ 'abcdefgh','abcdefgh', 'abcdefgh' /)

    print *, test_str
    print *, test_str(1:4)
    print *, test_str_array(1:4)
    print *, test_str_array(1:2)(1:4)
    print *, test_struct%test_str
    print *, test_struct%test_str(1:8)
    print *, test_struct%test_str(1:4)
    print *, test_struct%test_str_array(1:2)
    print *, test_struct%test_str_array(1:2)(2:2)
end program
