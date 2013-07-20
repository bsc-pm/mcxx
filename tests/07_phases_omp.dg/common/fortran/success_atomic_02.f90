! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    LOGICAL(1) :: L1_A, L1_B
    LOGICAL(2) :: L2_A, L2_B
    LOGICAL(4) :: L4_A, L4_B

    !----
       L1_A = .FALSE._1
       L1_B = .TRUE._1

       !$OMP ATOMIC
       L1_A = L1_A .EQV. L1_B
       IF (L1_A) CALL ABORT()
    !----

    !----
       L1_A = .FALSE._1
       L1_B = .TRUE._1

       !$OMP ATOMIC
       L1_A = L1_A .NEQV. L1_B
       IF (.NOT. L1_A) CALL ABORT()
    !----

    !----
       L2_A = .FALSE._2
       L2_B = .TRUE._2

       !$OMP ATOMIC
       L2_A = L2_A .EQV. L2_B
       IF (L2_A) CALL ABORT()
    !----

    !----
       L2_A = .FALSE._2
       L2_B = .TRUE._2

       !$OMP ATOMIC
       L2_A = L2_A .NEQV. L2_B
       IF (.NOT. L2_A) CALL ABORT()
    !----

    !----
       L4_A = .FALSE._4
       L4_B = .TRUE._4

       !$OMP ATOMIC
       L4_A = L4_A .EQV. L4_B
       IF (L4_A) CALL ABORT()
    !----

    !----
       L4_A = .FALSE._4
       L4_B = .TRUE._4

       !$OMP ATOMIC
       L4_A = L4_A .NEQV. L4_B
       IF (.NOT. L4_A) CALL ABORT()
    !----
END PROGRAM MAIN
