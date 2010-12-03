/*
<testinfo>
test_generator=config/mercurium-omp
test_noexec=yes

test_compile_fail_nanos4_plain=yes
test_compile_fail_nanox_plain=yes
test_compile_fail_nanox_instrument=yes
</testinfo>
*/

// fail
#pragma omp declare reduction( foo : float : _in *= _out )
