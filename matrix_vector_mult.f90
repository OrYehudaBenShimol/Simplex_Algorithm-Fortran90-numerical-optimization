MODULE MatrixVectorMultModule
  IMPLICIT none
  CONTAINS
        
SUBROUTINE matrix_vector_mult(C, A, B, n, m)
  INTEGER :: n, m
  DOUBLE PRECISION, DIMENSION(128, 128) :: A 
  DOUBLE PRECISION, DIMENSION(128) :: C, B
  INTEGER :: i, k
  DOUBLE PRECISION sum

  DO i = 1, n
    sum = 0
    DO k = 1, m
      sum = sum + A(i, k) * B(k)
    END DO
    C(i) = sum 
  END DO 
END SUBROUTINE matrix_vector_mult

END MODULE MatrixVectorMultModule 