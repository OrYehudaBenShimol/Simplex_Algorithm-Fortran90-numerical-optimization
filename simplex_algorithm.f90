MODULE SimplexAlgorithmModule
  USE BubbleSortModule
  USE ComputeCbCdModule
  USE SetDModule
  USE SetAAuxModule 
  USE CopySubmatrixModule
  USE EraseEpsilonMatrixModule
  USE EraseEpsilonVectorModule
  USE InvertMatrixModule
  USE MatrixMultModule
  USE MatrixVectorMultModule
  USE VectorMatrixMultModule
  USE VectorSubtractModule
  USE FindMinValueModule
  USE FindMostNegativeModule
  USE FindExitingIdModule

  IMPLICIT NONE
  CONTAINS

SUBROUTINE simplex_algorithm(A, A_aux, B, bb, basis, BI, BIA_aux, BIb, cb, cbBI, cbBID, cd, cc, D, dd, rd, n, m, epsilon)
  INTEGER :: n, m
  INTEGER, DIMENSION(128) :: basis, dd
  DOUBLE PRECISION, DIMENSION(128, 128) :: A, A_aux 
  DOUBLE PRECISION, DIMENSION(128, 128) :: B, BI, BIA_aux
  DOUBLE PRECISION, DIMENSION(128, 128) :: D
  DOUBLE PRECISION, DIMENSION(128) :: bb, BIb, cb, cbBI, cbBID
  DOUBLE PRECISION, DIMENSION(128) :: cc, cd, rd
  INTEGER :: i, j, k, optimal_flag, enter_id, exiting_id, itemp, basis_i
  DOUBLE PRECISION :: epsilon, dtemp, min_value
  INTEGER :: count

  count = 1
  optimal_flag = 0

  WRITE(*, '("m ="I2", Initial_n ="I2)') m, n
  WRITE(*,*) ''

  WRITE(*, '("basis1:")')  
  DO i = 1, m
    WRITE(*, '(" "I2" ")', ADVANCE='no') basis(i) 
  END DO
  WRITE(*,*) ''

  WRITE(*, '("A:")')  
  DO i = 1, m
    DO j = 1, n
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') A(i, j) 
    END DO
    WRITE(*,*) ''
  END DO
  WRITE(*,*) ''

  DO WHILE (optimal_flag == 0)
    WRITE(*, '("count = "I2)') count
    count = count + 1

    CALL bublesort(basis, m)

    WRITE(*,*) ''
    WRITE(*, '("basis2:")')
    DO i = 1, m
      WRITE(*, '(" "I2" ")', ADVANCE='no') basis(i)
    END DO
    WRITE(*,*) ''

    CALL set_d(basis, dd, n, m)

    WRITE(*,*) ''
    WRITE(*, '("")')
    DO i = 1, n
      WRITE(*, '(" "I2" ")', ADVANCE='no') dd(i)
    END DO
    WRITE(*,*) ''

    CALL set_A_aux(A, A_aux, dd, n, m)

    WRITE(*,*) ''
    WRITE(*, '("A_aux (B, D):")')  
    DO i = 1, m
      DO j = 1, n
        WRITE(*, '(" "F6.2" ")', ADVANCE='no') A_aux(i, j) 
      END DO
      WRITE(*,*) ''
    END DO
    WRITE(*,*) ''

        ! Set B
    CALL copy_submatrix(B, A_aux, 0, m, 0, m)

    WRITE(*,*) ''
    WRITE(*, '("B:")')  
    DO i = 1, m
      DO j = 1, m
        WRITE(*, '(" "F6.2" ")', ADVANCE='no') B(i, j) 
      END DO
      WRITE(*,*) ''
    END DO
    WRITE(*,*) ''

    !BI = B^-1
    CALL inv_gaussian(BI, B,  m, epsilon)   

    CALL erase_epsilons_matrix(BI, m, m, epsilon)

    WRITE(*,*) ''
    WRITE(*, '("BI:")')  
    DO i = 1, m
      DO j = 1, m
        WRITE(*, '(" "F6.2" ")', ADVANCE='no') BI(i, j) 
      END DO
      WRITE(*,*) ''
    END DO
    WRITE(*,*) ''

    CALL matrix_mult(BIA_aux, BI, A_aux, m, m, n)

    CALL erase_epsilons_matrix(BIA_aux, m, n, epsilon)
   
    WRITE(*, '("BIA_aux (I, B-1*D):")')  
    DO i = 1, m
      DO j = 1, n
        WRITE(*, '(" "F6.2" ")', ADVANCE='no') BIA_aux(i, j) 
      END DO
      WRITE(*,*) ''
    END DO
    WRITE(*,*) ''

    WRITE(*, '("A_aux (B,D):")')  
    DO i = 1, m
      DO j = 1, n
        WRITE(*, '(" "F6.2" ")', ADVANCE='no') A_aux(i, j) 
      END DO
      WRITE(*,*) ''
    END DO

    WRITE(*, '("b:")')  
    DO i = 1, m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') bb(i)
    END DO
    WRITE(*,*) ''

    CALL matrix_vector_mult(BIb, BI, bb,  m, m)
    CALL erase_epsilons_vector(BIb, m, epsilon)

    WRITE(*, '("BIb:")')  
    DO i = 1, m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') BIb(i)
    END DO
    WRITE(*,*) ''

    ! Set D
    CALL copy_submatrix(D, A_aux, 0, m, m, n - m) 

    WRITE(*, '("D:")')  
    DO i = 1, m
      DO j = 1, n - m
        WRITE(*, '(" "F6.2" ")', ADVANCE='no') D(i, j) 
      END DO
      WRITE(*,*) ''
    END DO
    ! END OF FOR DEBUG ONLY

    CALL compute_cb_cd(cb, cd, cc, dd, n, m)

    WRITE(*,*) ''
    WRITE(*, '("cb:")')  
    DO i = 1, m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') cb(i)
    END DO
    WRITE(*,*) ''

    WRITE(*,*) ''
    WRITE(*, '("cd:")')  
    DO i = 1, n - m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') cd(i)
    END DO
    WRITE(*,*) ''

    ! cbBI = cb * B^-1
    CALL vector_matrix_mult(cbBI, cb, BI, m, m) 
    CALL erase_epsilons_vector(cbBI, m, epsilon)

    WRITE(*,*) ''
    WRITE(*, '("cbBI:")')  
    DO i = 1, m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') cbBI(i)
    END DO
    WRITE(*,*) ''
 
    CALL vector_matrix_mult(cbBID, cbBI, D, m, n - m) 
    CALL erase_epsilons_vector(cbBID, n - m, epsilon)

    WRITE(*,*) ''
    WRITE(*, '("cbBID:")')  
    DO i = 1, n - m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') cbBID(i)
    END DO
    WRITE(*,*) ''

    CALL vector_subtract(rd, cd, cbBID, n - m)
    CALL erase_epsilons_vector(rd, n - m, epsilon)
 
    WRITE(*,*) ''
    WRITE(*, '("rd( cd - cbBID ):")')  
    DO i = 1, n - m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') rd(i)
    END DO
    WRITE(*,*) ''

    CALL find_min_value(rd, n, min_value)
    IF (min_value >= 0.0) THEN 
      optimal_flag = 1
    ELSE
      CALL find_most_negative(dd, rd, n, m, enter_id) 
      CALL find_exiting_id(dd, BIA_aux, BIb, enter_id, n, m, exiting_id)

      WRITE(*,*) ''
      WRITE(*, '("enter_id  = "I2",  exiting_id = "I2", d[exiting_id] = "I2)') enter_id - 1, &
        exiting_id - 1, dd(exiting_id)

      WRITE(*, '("pivot: enter_id = "I2", exiting_id = "I2)') enter_id - 1, dd(exiting_id)

      basis(exiting_id) = enter_id
      
      WRITE(*,*) ''
      WRITE(*, '("basis3:")')  
      DO i = 1, m
        WRITE(*, '(" "I2" ")', ADVANCE='no') basis(i)
      END DO
      WRITE(*,*) ''
    END IF
  END DO

END SUBROUTINE simplex_algorithm

END MODULE SimplexAlgorithmModule

