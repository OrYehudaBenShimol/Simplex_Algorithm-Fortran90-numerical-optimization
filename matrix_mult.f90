MODULE MatrixMultModule
  IMPLICIT none
  CONTAINS
        
SUBROUTINE matrix_mult(C, A, B, n, m, p)
  INTEGER :: n, m, p
  DOUBLE PRECISION, DIMENSION(128, 128) :: A, B, C
  INTEGER :: i, j, k
  DOUBLE PRECISION sum

  DO i = 1, n
    DO j = 1, p
      sum = 0
      DO k = 1, m
        sum = sum + A(i, k) * B(k, j)
      END DO
      C(i, j) = sum 
    END DO
  END DO  
END SUBROUTINE matrix_mult

END MODULE MatrixMultModule 