MODULE EraseEpsilonVectorModule
  IMPLICIT none
  CONTAINS
        
SUBROUTINE erase_epsilons_vector(darray, n, epsilon)
  INTEGER :: n
  DOUBLE PRECISION, DIMENSION(128) :: darray
  DOUBLE PRECISION epsilon
  INTEGER :: i, j

  DO i = 1, n
    IF (ABS(darray(i)) < epsilon) THEN
      darray(i) = 0.0 
    END IF
  END DO
  
END SUBROUTINE erase_epsilons_vector

END MODULE EraseEpsilonVectorModule
