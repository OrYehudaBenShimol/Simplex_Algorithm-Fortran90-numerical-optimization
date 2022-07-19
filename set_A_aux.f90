! Swap columns

MODULE SetAAuxModule
  IMPLICIT none
  CONTAINS

SUBROUTINE set_A_aux(A, A_aux, d, n, m)
  INTEGER :: n, m
  INTEGER, DIMENSION(128) :: d
  DOUBLE PRECISION, DIMENSION(128, 128) :: A, A_aux
  INTEGER :: i, j, k

  DO i=1, n
    k = d(i) + 1
    DO j=1, m
      A_aux(j, i) = A(j, k)
    END DO
  END DO

END SUBROUTINE set_A_aux

END MODULE SetAAuxModule
