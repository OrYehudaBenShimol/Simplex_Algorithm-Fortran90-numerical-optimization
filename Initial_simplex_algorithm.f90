MODULE InitialSimplexAlgorithm
  USE BubbleSortModule
  USE ComputeInitialCbInitialCdModule
  USE InitialSetDModule
  USE SetInitialAAuxModule 
  USE CopySubmatrixModule
  USE EraseEpsilonMatrixModule
  USE EraseEpsilonVectorModule
  USE InvertMatrixModule
  USE MatrixMultModule
  USE MatrixVectorMultModule
  USE VectorMatrixMultModule
  USE VectorSubtractModule
  USE FindMinValueModule
  USE FindInitialMostNegativeModule
  USE FindInitialExitingIdModule
  IMPLICIT none
  CONTAINS
        
SUBROUTINE Initial_simplex_algorithm(Initial_A, Initial_A_aux, Initial_B, Initial_bb, Initial_basis, &
             Initial_BI, Initial_BIA_aux, Initial_BIb, Initial_cb, Initial_cbBI, Initial_cbBID, Initial_cd, &
             Initial_cc, Initial_D, Initial_dd, Initial_rd, Initial_n, n, m, epsilon)
  INTEGER :: Initial_n, n, m
  INTEGER, DIMENSION(128) :: Initial_basis, Initial_dd
  DOUBLE PRECISION, DIMENSION(128, 128) :: Initial_A, Initial_A_aux 
  DOUBLE PRECISION, DIMENSION(128, 128) :: Initial_B, Initial_BI, Initial_BIA_aux
  DOUBLE PRECISION, DIMENSION(128, 128) :: Initial_D
  DOUBLE PRECISION, DIMENSION(128) :: Initial_bb, Initial_BIb, Initial_cb, Initial_cbBI, Initial_cbBID
  DOUBLE PRECISION, DIMENSION(128) :: Initial_cc, Initial_cd, Initial_rd
  INTEGER :: i, j, k, optimal_flag, enter_id,  exiting_id, itemp, basis_i
  DOUBLE PRECISION :: epsilon, dtemp, min_value
  INTEGER :: count

  count = 0
  optimal_flag = 0

  WRITE(*, '("m ="I2", Initial_n ="I2)') m, Initial_n
  WRITE(*,*) ''

  WRITE(*, '("Initial_A:")')  
  DO i = 1, m
    DO j = 1, Initial_n
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_A(i, j) 
    END DO
    WRITE(*,*) ''
  END DO
  WRITE(*,*) ''

  DO WHILE (optimal_flag == 0)

    CALL bublesort(Initial_basis, m)

    WRITE(*,*) ''
    WRITE(*, '("Initial_basis:")')
    DO i = 1, m
      WRITE(*, '(" "I2" ")', ADVANCE='no') Initial_basis(i)
    END DO
    WRITE(*,*) ''

    CALL Initial_set_d(Initial_basis, Initial_dd, Initial_n, m)

    WRITE(*,*) ''
    WRITE(*, '("Initial_d:")')
    DO i = 1, Initial_n
      WRITE(*, '(" "I2" ")', ADVANCE='no') Initial_dd(i)
    END DO
    WRITE(*,*) ''

    CALL set_Initial_A_aux(Initial_A, Initial_A_aux, Initial_dd, Initial_n, m)

    WRITE(*,*) ''
    WRITE(*, '("Initial_A_aux (B, D):")')  
    DO i = 1, m
      DO j = 1, Initial_n
        WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_A_aux(i, j) 
      END DO
      WRITE(*,*) ''
    END DO
    WRITE(*,*) ''

    ! Set B
    CALL copy_submatrix(Initial_B, Initial_A_aux, 0, m, 0, m)

    WRITE(*,*) ''
    WRITE(*, '("Initial_B:")')  
    DO i = 1, m
      DO j = 1, m
        WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_B(i, j) 
      END DO
      WRITE(*,*) ''
    END DO
    WRITE(*,*) ''

    !BI = B^-1
    CALL inv_gaussian(Initial_BI, Initial_B,  m, epsilon)   

    CALL erase_epsilons_matrix(Initial_BI, m, m, epsilon)

    WRITE(*,*) ''
    WRITE(*, '("Initial_BI")')  
    DO i = 1, m
      DO j = 1, m
        WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_BI(i, j) 
      END DO
      WRITE(*,*) ''
    END DO
    WRITE(*,*) ''

    CALL matrix_mult(Initial_BIA_aux, Initial_BI, Initial_A_aux, m, m, Initial_n)

    CALL erase_epsilons_matrix(Initial_BIA_aux, m, Initial_n, epsilon)
   
    WRITE(*, '("Initial_BIA_aux (I, B-1*D):")')  
    DO i = 1, m
      DO j = 1, Initial_n
        WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_BIA_aux(i, j) 
      END DO
      WRITE(*,*) ''
    END DO
    WRITE(*,*) ''

    WRITE(*, '("Initial_A_aux (B,D):")')  
    DO i = 1, m
      DO j = 1, Initial_n
        WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_A_aux(i, j) 
      END DO
      WRITE(*,*) ''
    END DO

    WRITE(*, '("Initial_b:")')  
    DO i = 1, m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_bb(i)
    END DO
    WRITE(*,*) ''

    CALL matrix_vector_mult(Initial_BIb, Initial_BI, Initial_bb,  m, m)
    CALL erase_epsilons_vector(Initial_BIb, m, epsilon)

    WRITE(*, '("Initial_BIb:")')  
    DO i = 1, m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_BIb(i)
    END DO
    WRITE(*,*) ''

    ! Set Initial_D
    CALL copy_submatrix(Initial_D, Initial_A_aux, 0, m, m, Initial_n - m) 

    WRITE(*, '("Initial_D:")')  
    DO i = 1, m
      DO j = 1, Initial_n - m
        WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_D(i, j) 
      END DO
      WRITE(*,*) ''
    END DO
    ! END OF FOR DEBUG ONLY

    CALL compute_Initial_cb_Initial_cd(Initial_cb, Initial_cd, Initial_cc, Initial_dd, Initial_n, m)

    WRITE(*,*) ''
    WRITE(*, '("Initial_cb:")')  
    DO i = 1, m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_cb(i)
    END DO
    WRITE(*,*) ''

    WRITE(*,*) ''
    WRITE(*, '("Initial_cd:")')  
    DO i = 1, Initial_n - m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_cd(i)
    END DO
    WRITE(*,*) ''

   ! cbBI = cb * B^-1
    CALL vector_matrix_mult(Initial_cbBI, Initial_cb, Initial_BI, m, m) 
    CALL erase_epsilons_vector(Initial_cbBI, m, epsilon)

    WRITE(*,*) ''
    WRITE(*, '("Initial_cbBI:")')  
    DO i = 1, m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_cbBI(i)
    END DO
    WRITE(*,*) ''
 
    CALL vector_matrix_mult(Initial_cbBID, Initial_cbBI, Initial_D, m, Initial_n - m) 
    CALL erase_epsilons_vector(Initial_cbBID, Initial_n - m, epsilon)

    WRITE(*,*) ''
    WRITE(*, '("Initial_cbBID:")')  
    DO i = 1, Initial_n - m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_cbBID(i)
    END DO
    WRITE(*,*) ''

    CALL vector_subtract(Initial_rd, Initial_cd, Initial_cbBID, Initial_n - m)
    CALL erase_epsilons_vector(Initial_rd, Initial_n - m, epsilon)
 
    WRITE(*,*) ''
    WRITE(*, '("Initial_rd( cd - cbBID ):")')  
    DO i = 1, Initial_n - m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_rd(i)
    END DO
    WRITE(*,*) ''

    CALL find_min_value(Initial_rd, n, min_value)
    IF (min_value >= 0.0) THEN 
      optimal_flag = 1
    ELSE
      CALL find_Initial_most_negative(Initial_dd, Initial_rd, Initial_n, m, enter_id) 
      CALL find_Initial_exiting_id(Initial_dd, Initial_n, Initial_BIA_aux, Initial_BIb, enter_id, Initial_n, m, exiting_id)

      WRITE(*,*) ''
      WRITE(*, '("enter_id  = "I2",  exiting_id = "I2",  Initial_d[exiting_id] = "I2)') enter_id - 1, &
        exiting_id - 1, Initial_dd(exiting_id)

      Initial_basis(exiting_id) = enter_id
      
      WRITE(*,*) ''
      WRITE(*, '("Initial_basis:")')  
      DO i = 1, m
        WRITE(*, '(" "I2" ")', ADVANCE='no') Initial_basis(i)
      END DO
      WRITE(*,*) ''

    END IF
  END DO  
  
END SUBROUTINE Initial_simplex_algorithm

END MODULE InitialSimplexAlgorithm