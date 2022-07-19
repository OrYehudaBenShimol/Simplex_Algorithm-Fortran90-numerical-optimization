MODULE VectorMatrixMultModule
  IMPLICIT none
  CONTAINS
        
SUBROUTINE vector_matrix_mult(C, B, A, n, m)
  INTEGER :: n, m
  DOUBLE PRECISION, DIMENSION(128) :: C, B
  DOUBLE PRECISION, DIMENSION(128, 128) :: A 
  INTEGER :: i, k
  DOUBLE PRECISION sum

  DO i = 1, m
    sum = 0
    DO k = 1, n
      sum = sum + A(k, i) * B(k)
    END DO
    C(i) = sum 
  END DO 
END SUBROUTINE vector_matrix_mult

END MODULE VectorMatrixMultModule 