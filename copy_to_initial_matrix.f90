MODULE CopyToInitialMatrix
  IMPLICIT none
  CONTAINS
        
SUBROUTINE copy_to_initial_matrix(A, Initial_A, Initial_A_aux, n, m)
  INTEGER :: n, m
  DOUBLE PRECISION, DIMENSION(128, 128) :: A, Initial_A, Initial_A_aux  
  INTEGER :: i, j

  DO i = 1, m
    DO j = 1, n
      Initial_A(i, j) = A(i, j)
      Initial_A_aux(i, j) = A(i, j)
    END DO
  END DO

  DO i = 1, m
    DO j = n + 1, n + m
      IF (i == (j - n)) THEN 
        Initial_A(i, j) = 1.0
        Initial_A_aux(i, j) = 1.0
      ELSE
        Initial_A(i, j) = 0.0
        Initial_A_aux(i, j) = 0.0
      END IF
    END DO
  END DO
  
END SUBROUTINE copy_to_initial_matrix

END MODULE CopyToInitialMatrix