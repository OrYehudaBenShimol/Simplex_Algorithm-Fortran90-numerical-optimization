! Swap columns

MODULE SetInitialAAuxModule
  IMPLICIT none
  CONTAINS

SUBROUTINE set_Initial_A_aux(Initial_A, Initial_A_aux, Initial_d, Initial_n, m)
  INTEGER :: Initial_n, m
  INTEGER, DIMENSION(128) :: Initial_d
  DOUBLE PRECISION, DIMENSION(128, 128) :: Initial_A, Initial_A_aux
  INTEGER :: i, j, k

  DO i=1, Initial_n
    k = Initial_d(i) + 1
    DO j=1, m
      Initial_A_aux(j, i) = Initial_A(j, k)
    END DO
  END DO

END SUBROUTINE set_Initial_A_aux

END MODULE SetInitialAAuxModule
