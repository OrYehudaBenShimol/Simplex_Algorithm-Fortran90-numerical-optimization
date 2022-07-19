MODULE SwapRowsModule
  IMPLICIT none
  CONTAINS

SUBROUTINE swap_rows(W, n, m1, m2)
  INTEGER :: n, m1, m2
  DOUBLE PRECISION, DIMENSION(128, 128) :: W
  INTEGER :: i, j
  DOUBLE PRECISION temp

  DO i = 1, 2 * n
    temp = W(m1, i)
    W(m1, i) = W(m2, i)
    W(m2, i) = temp 
  END DO
  
END SUBROUTINE swap_rows

END MODULE SwapRowsModule