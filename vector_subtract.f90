MODULE VectorSubtractModule
  IMPLICIT none
  CONTAINS

SUBROUTINE vector_subtract(result_v, v1, v2, n)
  INTEGER :: n
  DOUBLE PRECISION, DIMENSION(128) :: result_v, v1, v2
  INTEGER :: i

  DO i = 1, n
    result_v(i) = v1(i) - v2(i)
  END DO

END SUBROUTINE vector_subtract

END MODULE VectorSubtractModule
