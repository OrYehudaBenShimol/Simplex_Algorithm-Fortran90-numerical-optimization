MODULE FindInitialMostNegativeModule
  IMPLICIT none
  CONTAINS

! Assumptions: d[i] is original index, rd orderred by i=0,.., n-1 
!                                                  d[0] ... d[n-1]
SUBROUTINE find_Initial_most_negative(Initial_d, Initial_rd, Initial_n, m, most_index)
  INTEGER, DIMENSION(128) :: Initial_d 
  DOUBLE PRECISION, DIMENSION(128) :: Initial_rd
  INTEGER :: Initial_n, m, most_index, i
  DOUBLE PRECISION :: temp_value

  most_index = Initial_d(m + 1)
  temp_value = Initial_rd(1)
  DO i = 1, Initial_n - m
    IF (Initial_rd(i) < temp_value) THEN
      most_index = Initial_d(m + i)
      temp_value = Initial_rd(i)
    END IF
  END DO

END SUBROUTINE find_Initial_most_negative 

END MODULE FindInitialMostNegativeModule