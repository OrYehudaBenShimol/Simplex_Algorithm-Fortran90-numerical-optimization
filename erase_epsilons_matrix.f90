MODULE EraseEpsilonMatrixModule
  IMPLICIT none
  CONTAINS
        
SUBROUTINE erase_epsilons_matrix(dmat, n, m, epsilon)
  INTEGER :: n, m
  DOUBLE PRECISION, DIMENSION(128, 128) :: dmat
  DOUBLE PRECISION epsilon
  INTEGER :: i, j

  DO i = 1, m
    DO j = 1, n
      IF (ABS(dmat(i, j)) < epsilon) THEN
        dmat(i, j) = 0.0 
      END IF
    END DO
  END DO
  
END SUBROUTINE erase_epsilons_matrix

END MODULE EraseEpsilonMatrixModule
